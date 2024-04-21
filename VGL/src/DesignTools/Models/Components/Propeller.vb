﻿'#############################################################################
'VGL
'Open source aeromechanics in dotnet
'Copyright (C) 2023 Guillermo Hazebrouck (gahazebrouck@gmail.com)

'This program Is free software: you can redistribute it And/Or modify
'it under the terms Of the GNU General Public License As published by
'the Free Software Foundation, either version 3 Of the License, Or
'(at your option) any later version.

'This program Is distributed In the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty Of
'MERCHANTABILITY Or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License For more details.

'You should have received a copy Of the GNU General Public License
'along with this program.  If Not, see < http:  //www.gnu.org/licenses/>.

'' Standard .NET frameworks dependencies
'-----------------------------------------------------------------------------
Imports System.IO
Imports System.Xml

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.AeroTools.Models.Aero.Components
Imports VGL.AeroTools.IoHelper
Imports VGL.AeroTools.Settings
Imports VGL.DesignTools.Interface
Imports VGL.DesignTools.Models.Components.Basics
Imports VGL.DesignTools.DataStore
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports VGL.MathTools.Algebra.CustomMatrices

'#############################################################################
' Unit: Propeller
'
' This unit provides a propeller model for any number of blades. The geometry  
' of the blades is generated using the functions that describe the distribution
' of twist and chord along the span (the usual geometry representation available
' on databases). The position coordinate given to the functions is considered 
' as normalized with respect to the radius of the propeller.
' The shape functions are a linear interpolation of provided nodes.
' Take into acconunt that the range of the functions must be from 0 to 1 or 
' there will be troubles while meshing. This range is not checked by the
' mesher.
' The shape of the camber line of the airfoil is constant along the span and
' there is only one polar family (a set of Reynolds dependent polars).
' The collective pitch can be adjusted as independent variable.
'#############################################################################
Namespace DesignTools.Models.Components

    Public Class Propeller

        Inherits Surface

        ''' <summary>
        ''' Initializes the propeller using default values
        ''' </summary>
        Public Sub New()

            VisualProperties = New VisualProperties(ComponentTypes.etPropeller)
            VisualProperties.ShowSurface = True
            VisualProperties.ShowMesh = True
            IncludeInCalculation = True

            Mesh = New Mesh
            NumberOfBlades = 2
            NumberOfChordPanels = 5
            NumberOfSpanPanels = 20
            Diameter = 2.0#
            CollectivePitch = 0.0#
            AxisDeflection = 0.2#
            AxisPosition = 0.25#
            CuttingStep = 50
            TwistFunction.Add(New Vector2(0.0, 45.0))
            TwistFunction.Add(New Vector2(1.0, 15.0))
            ChordFunction.Add(New Vector2(0.1, 0.25))
            ChordFunction.Add(New Vector2(1.0, 0.1))

            GenerateMesh()

        End Sub

        ''' <summary>
        ''' Position of the local origin in global coordinates.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property LocalOrigin As New Vector3

        ''' <summary>
        ''' Cached points used to represent the directions of the local axes.
        ''' </summary>
        Public Property MainDirections As New Base3

        ''' <summary>
        ''' Index of polar curve to be loaded.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property PolarId As Guid

        ''' <summary>
        ''' Local polar curve.
        ''' </summary>
        Public Property PolarFamiliy As PolarFamily

#Region " Parametric geometry "

        ''' <summary>
        ''' The diameter of the propeller
        ''' </summary>
        Public Property Diameter As Double

        ''' <summary>
        ''' The collective pitch angle
        ''' </summary>
        Public Property CollectivePitch As Double

        ''' <summary>
        ''' The bending along the blade axis
        ''' </summary>
        Public Property AxisDeflection As Double

        ''' <summary>
        ''' The position of the axis relative to the leading edge
        ''' </summary>
        Public Property AxisPosition As Double

        ''' <summary>
        ''' Index of polar curve to be loaded.
        ''' </summary>
        Public Property CamberLineId As Guid

        ''' <summary>
        ''' The number of blades
        ''' </summary>
        Public Property NumberOfBlades As Integer

        ''' <summary>
        '''  The number of panels along the span
        ''' </summary>
        Public Property NumberOfSpanPanels As Integer

        ''' <summary>
        ''' The number of panels along the chord
        ''' </summary>
        Public Property NumberOfChordPanels As Integer

        ''' <summary>
        ''' The wake cutting step
        ''' </summary>
        Public Property CuttingStep As Integer

        ''' <summary>
        ''' The nodes describing the twist of the blade along the normalized coordinate
        ''' </summary>
        Public Property TwistFunction As New List(Of Vector2)

        ''' <summary>
        ''' Returns the twist at an specific normalized coordinate (rande from 0 to 1)
        ''' </summary>
        Function Twist(X As Double) As Double

            For i = 0 To TwistFunction.Count - 2

                If X >= TwistFunction(i).X And X <= TwistFunction(i + 1).X Then

                    Return TwistFunction(i).Y + (TwistFunction(i + 1).Y - TwistFunction(i).Y) * (X - TwistFunction(i).X) / (TwistFunction(i + 1).X - TwistFunction(i).X)

                End If

            Next

            Return 0.0#

        End Function

        ''' <summary>
        ''' The nodes describing the chord of the blade along the normalized coordinate
        ''' </summary>
        Public Property ChordFunction As New List(Of Vector2)

        ''' <summary>
        ''' Returns the twist at an specific normalized coordinate (rande from 0 to 1)
        ''' </summary>
        Function Chord(X As Double) As Double

            For i = 0 To ChordFunction.Count - 2

                If X >= ChordFunction(i).X And X <= ChordFunction(i + 1).X Then

                    Return ChordFunction(i).Y + (ChordFunction(i + 1).Y - ChordFunction(i).Y) * (X - ChordFunction(i).X) / (ChordFunction(i + 1).X - ChordFunction(i).X)

                End If

            Next

            Return 0.0#

        End Function

#End Region

#Region " 3D model and vortices generation "

        ''' <summary>
        ''' Generates the lifting surface mesh based on the geometrical parameters.
        ''' </summary>
        Public Overrides Sub GenerateMesh()

            Mesh.Nodes.Clear()
            Mesh.Panels.Clear()
            Mesh.Lattice.Clear()

            ' Load quad panels (base on indices only)
            '---------------------------------------------------------------------

            Dim NumberOfChordNodes As Integer = NumberOfChordPanels + 1
            Dim NumberOfSpanNodes As Integer = NumberOfSpanPanels + 1
            Dim NumberOfPanels As Integer = NumberOfChordPanels * NumberOfSpanPanels
            Dim NumberOfNodes As Integer = NumberOfChordNodes * NumberOfSpanNodes

            For K = 1 To NumberOfBlades

                Dim LastCount As Integer = (K - 1) * NumberOfNodes

                For I = 1 To NumberOfSpanPanels

                    For J = 0 To NumberOfChordPanels - 1

                        Dim Panel As New Panel

                        Panel.N1 = (I - 1) * NumberOfChordNodes + J + LastCount
                        Panel.N2 = (I - 1) * NumberOfChordNodes + J + 1 + LastCount
                        Panel.N3 = I * NumberOfChordNodes + J + 1 + LastCount
                        Panel.N4 = I * NumberOfChordNodes + J + LastCount

                        Panel.IsSlender = True
                        Panel.IsReversed = False
                        Panel.IsPrimitive = (J = NumberOfChordPanels - 1)

                        Mesh.Panels.Add(Panel)

                    Next

                Next

            Next

            ' Load nodes (base on parameters)
            '---------------------------------------------------------------------

            Dim Camber As CamberLine = GetCamberLineFromId(CamberLineId)

            Dim Xmin As Double = ChordFunction(0).X

            For K = 1 To NumberOfBlades

                Dim F As Double = (K - 1) / NumberOfBlades * 2.0# * Math.PI

                For I = 0 To NumberOfSpanPanels

                    Dim S As Double = Xmin + (1.0# - Xmin) * CDbl(I) / CDbl(NumberOfSpanPanels)
                    Dim R As Double = 0.5# * S * Diameter
                    Dim C As Double = 0.5# * Diameter * Chord(S)
                    Dim T As Double = (CollectivePitch + Twist(S)) * Math.PI / 180.0#
                    Dim E As Double = 0.5# * AxisDeflection * Diameter * Math.Sin(Math.PI * S)

                    For J = 0 To NumberOfChordPanels

                        Dim Q As Double = J / NumberOfChordPanels
                        Dim B As Double = 0.0#
                        If Camber IsNot Nothing Then
                            B = Camber.Y(Q)
                        End If
                        Dim Node As New NodalPoint
                        Mesh.Nodes.Add(Node)

                        ' Apply profile

                        Node.Position.X = -C * B
                        Node.Position.Y = 0.0#
                        Node.Position.Z = C * Q

                        ' Apply twist (for now only around C/2)

                        Node.Position.Z -= C * (1.0 - AxisPosition)

                        Dim M As New RotationMatrix()
                        Dim A As New OrientationAngles
                        A.Angle1 = 0.0#
                        A.Angle2 = T
                        A.Angle3 = 0.0#
                        A.Sequence = RotationSequence.XYZ
                        M.Generate(A)

                        Node.Position.Rotate(M)

                        ' Apply local span position

                        Node.Position.Y += R

                        ' Apply sinusolidal bending

                        Node.Position.Z += E

                        ' Apply blade rotation around proppeller axis (X)

                        A.Angle1 = F
                        A.Angle2 = 0.0#
                        A.Angle3 = 0.0#
                        A.Sequence = RotationSequence.XYZ
                        M.Generate(A)

                        Node.Position.Rotate(M)

                    Next

                Next

            Next

            Mesh.GenerateLattice()

        End Sub

#End Region

        ''' <summary>
        ''' Loads the geometry of the propeller from a text file
        ''' The format of each line must be: r/R c/R beta
        ''' </summary>
        Public Sub LoadFromFile(FilePath As String)

            If File.Exists(FilePath) Then

                Dim FileId = FreeFile()

                FileOpen(FileId, FilePath, OpenMode.Input)

                TwistFunction.Clear()
                ChordFunction.Clear()

                While Not EOF(FileId)

                    Dim Line As String = LineInput(FileId)

                    If Line.Length > 0 AndAlso Line(0) <> "#" Then

                        Dim Values As String() = Line.Split({" "c}, StringSplitOptions.RemoveEmptyEntries)

                        If Values.Length = 3 Then

                            Dim R As Double = CDbl(Values(0))

                            ChordFunction.Add(New Vector2(R, CDbl(Values(1))))

                            TwistFunction.Add(New Vector2(R, CDbl(Values(2))))

                        End If

                    End If

                End While

                FileClose(FileId)

                GenerateMesh()

            End If

        End Sub

        ''' <summary>
        ''' Generates an identical propeller
        ''' </summary>
        Public Overrides Function Clone() As Surface

            Throw New NotImplementedException()

        End Function

        ''' <summary>
        ''' Encodes the function as a string
        ''' </summary>
        Function EncodePoints(Points As List(Of Vector2)) As String

            Dim Result As String = ""

            For Each Point In Points

                Result += String.Format("{0:F4};{1:F4}/", Point.X, Point.Y)

            Next

            Return Result

        End Function

        ''' <summary>
        ''' Encodes the function as a string
        ''' </summary>
        Sub DecodePoints(Data As String, ByRef Points As List(Of Vector2))

            Points.Clear()

            Dim Coords As String() = Data.Split({"/"c}, StringSplitOptions.RemoveEmptyEntries)

            For Each Coord In Coords

                Dim XY As String() = Coord.Split({";"c}, StringSplitOptions.RemoveEmptyEntries)

                If XY.Length = 2 Then

                    Dim Point As New Vector2

                    Point.X = CDbl(XY(0))
                    Point.Y = CDbl(XY(1))

                    Points.Add(Point)

                End If

            Next

        End Sub

        ''' <summary>
        ''' Writes the data to an XML node
        ''' </summary>
        Public Overrides Sub WriteToXML(ByRef Writer As XmlWriter)

            ' Identity
            '-----------------------------------------------------

            Writer.WriteStartElement("Identity")
            Writer.WriteAttributeString("Name", Name)
            Writer.WriteAttributeString("ID", Id.ToString)
            Writer.WriteAttributeString("Include", String.Format("{0}", IncludeInCalculation))

            Writer.WriteAttributeString("DI", CDbl(Diameter))
            Writer.WriteAttributeString("CP", CDbl(CollectivePitch))
            Writer.WriteAttributeString("DF", CDbl(AxisDeflection))
            Writer.WriteAttributeString("PS", CDbl(AxisPosition))
            Writer.WriteAttributeString("NB", CInt(NumberOfBlades))
            Writer.WriteAttributeString("CI", CamberLineId.ToString)
            Writer.WriteAttributeString("PI", PolarId.ToString)
            Writer.WriteAttributeString("NC", CInt(NumberOfChordPanels))
            Writer.WriteAttributeString("NS", CInt(NumberOfSpanPanels))
            Writer.WriteAttributeString("CS", CInt(CuttingStep))
            Writer.WriteAttributeString("TF", EncodePoints(TwistFunction))
            Writer.WriteAttributeString("CF", EncodePoints(ChordFunction))

            Writer.WriteAttributeString("X", CDbl(Position.X))
            Writer.WriteAttributeString("Y", CDbl(Position.Y))
            Writer.WriteAttributeString("Z", CDbl(Position.Z))

            Writer.WriteAttributeString("Psi", CDbl(Orientation.Angle1))
            Writer.WriteAttributeString("Theta", CDbl(Orientation.Angle2))
            Writer.WriteAttributeString("Phi", CDbl(Orientation.Angle3))
            Writer.WriteAttributeString("Sequence", String.Format("{0}", CInt(Orientation.Sequence)))

            Writer.WriteAttributeString("Xcr", String.Format("{0}", Position.X))
            Writer.WriteAttributeString("Ycr", String.Format("{0}", Position.Y))
            Writer.WriteAttributeString("Zcr", String.Format("{0}", Position.Z))

            Writer.WriteEndElement()

            ' Visual properties
            '-----------------------------------------------------

            Writer.WriteStartElement("VisualProperties")
            VisualProperties.WriteToXML(Writer)
            Writer.WriteEndElement()

            ' Inertia
            '-----------------------------------------------------

            Writer.WriteStartElement("Inertia")

            Writer.WriteAttributeString("Mass", String.Format("{0,14:E6}", Inertia.Mass))

            Writer.WriteAttributeString("Xcg", String.Format("{0,14:E6}", Inertia.Xcg))
            Writer.WriteAttributeString("Ycg", String.Format("{0,14:E6}", Inertia.Ycg))
            Writer.WriteAttributeString("Zcg", String.Format("{0,14:E6}", Inertia.Zcg))

            Writer.WriteAttributeString("Ixx", String.Format("{0,14:E6}", Inertia.Ixx))
            Writer.WriteAttributeString("Iyy", String.Format("{0,14:E6}", Inertia.Iyy))
            Writer.WriteAttributeString("Izz", String.Format("{0,14:E6}", Inertia.Izz))

            Writer.WriteAttributeString("Ixy", String.Format("{0,14:E6}", Inertia.Ixy))
            Writer.WriteAttributeString("Ixz", String.Format("{0,14:E6}", Inertia.Ixz))
            Writer.WriteAttributeString("Iyz", String.Format("{0,14:E6}", Inertia.Iyz))

            Writer.WriteEndElement()

        End Sub

        ''' <summary>
        ''' Reads the data from an XML node
        ''' </summary>
        Public Overrides Sub ReadFromXML(ByRef Reader As XmlReader)

            While Reader.Read

                Select Case Reader.Name

                    Case "Identity"

                        Name = Reader.GetAttribute("Name")
                        Id = New Guid(IOXML.ReadString(Reader, "ID", Guid.NewGuid.ToString))
                        IncludeInCalculation = IOXML.ReadBoolean(Reader, "Include", True)

                        Diameter = IOXML.ReadDouble(Reader, "DI", 1.0)
                        CollectivePitch = IOXML.ReadDouble(Reader, "CP", 0.0#)
                        AxisDeflection = IOXML.ReadDouble(Reader, "DF", 0.0#)
                        AxisPosition = IOXML.ReadDouble(Reader, "PS", 0.25#)
                        NumberOfBlades = IOXML.ReadInteger(Reader, "NB", 0)
                        CamberLineId = Guid.Parse(IOXML.ReadString(Reader, "CI", Guid.Empty.ToString))
                        PolarId = Guid.Parse(IOXML.ReadString(Reader, "PI", Guid.Empty.ToString))
                        NumberOfChordPanels = IOXML.ReadInteger(Reader, "NC", 5)
                        NumberOfSpanPanels = IOXML.ReadInteger(Reader, "NS", 20)
                        CuttingStep = IOXML.ReadInteger(Reader, "CS", 50)
                        DecodePoints(IOXML.ReadString(Reader, "TF", ""), TwistFunction)
                        DecodePoints(IOXML.ReadString(Reader, "CF", ""), ChordFunction)

                        Position.X = IOXML.ReadDouble(Reader, "X", 0.0)
                        Position.Y = IOXML.ReadDouble(Reader, "Y", 0.0)
                        Position.Z = IOXML.ReadDouble(Reader, "Z", 0.0)

                        Orientation.Angle1 = IOXML.ReadDouble(Reader, "Psi", 0)
                        Orientation.Angle2 = IOXML.ReadDouble(Reader, "Theta", 0)
                        Orientation.Angle3 = IOXML.ReadDouble(Reader, "Phi", 0)
                        Orientation.Sequence = IOXML.ReadInteger(Reader, "Sequence", CInt(RotationSequence.ZYX))

                        CenterOfRotation.X = IOXML.ReadDouble(Reader, "Xcr", 0.0)
                        CenterOfRotation.Y = IOXML.ReadDouble(Reader, "Ycr", 0.0)
                        CenterOfRotation.Z = IOXML.ReadDouble(Reader, "Zcr", 0.0)

                    Case "VisualProperties"

                        VisualProperties.ReadFromXML(Reader.ReadSubtree)

                    Case "Inertia"

                        Dim I As InertialProperties

                        I.Mass = IOXML.ReadDouble(Reader, "Mass", 0.0)

                        I.Xcg = IOXML.ReadDouble(Reader, "Xcg", 0.0)
                        I.Ycg = IOXML.ReadDouble(Reader, "Ycg", 0.0)
                        I.Zcg = IOXML.ReadDouble(Reader, "Zcg", 0.0)

                        I.Ixx = IOXML.ReadDouble(Reader, "Ixx", 0.0)
                        I.Iyy = IOXML.ReadDouble(Reader, "Iyy", 0.0)
                        I.Izz = IOXML.ReadDouble(Reader, "Izz", 0.0)

                        I.Ixy = IOXML.ReadDouble(Reader, "Ixy", 0.0)
                        I.Ixz = IOXML.ReadDouble(Reader, "Ixz", 0.0)
                        I.Iyz = IOXML.ReadDouble(Reader, "Iyz", 0.0)

                        Inertia = I

                End Select

            End While

            GenerateMesh()

        End Sub

    End Class

End Namespace

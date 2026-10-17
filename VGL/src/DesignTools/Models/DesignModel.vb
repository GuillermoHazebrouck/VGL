'VGL
'Open source aeromechanics in dotnet
'Copyright (C) 2026 Guillermo Hazebrouck (gahazebrouck@gmail.com)

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
Imports System.Xml

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.AeroTools.Models.Aero.Components
Imports VGL.AeroTools.Settings
Imports VGL.DesignTools.Interface
Imports VGL.DesignTools.Models.Components
Imports VGL.DesignTools.Models.Components.Basics
Imports VGL.DesignTools.DataStore

'#############################################################################
' Unit: DesignModel
'
' This unit provides design model containing the different components 
'#############################################################################
Namespace DesignTools.Models

    Public Class DesignModel

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Project name.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Name As String

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' List of model objects.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Objects As New List(Of Surface)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Tool that provides back up info while operating (moving or aligning objects).
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property OperationsTool As New OperationsTool

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Polars.
        '//////////////////////////////////////////////////////////////////////////////////////////
        
        Public Property PolarDataBase As New PolarDatabase

#Region " Add and clone objects "

        '==========================================================================================
        ' Adds a lifting surface and sets it as current.
        '==========================================================================================
        Public Sub AddLiftingSurface()

            Dim NewLiftingSurface = New LiftingSurface
            NewLiftingSurface.Name = String.Format("Wing - {0}", Objects.Count)
            Objects.Add(NewLiftingSurface)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds an extruded body
        '==========================================================================================
        Public Sub AddExtrudedBody()

            Dim NewFuselage As New Fuselage
            NewFuselage.Name = String.Format("Fuselage - {0}", Objects.Count)
            NewFuselage.IncludeInCalculation = True
            NewFuselage.Position.SetToCero()
            NewFuselage.CenterOfRotation.SetToCero()
            NewFuselage.Orientation.SetToZero()
            NewFuselage.VisualProperties.ShowColormap = False
            NewFuselage.VisualProperties.ShowLoadVectors = False
            NewFuselage.VisualProperties.ShowNodes = False
            NewFuselage.VisualProperties.ShowVelocityVectors = False
            NewFuselage.VisualProperties.ShowPrimitives = False
            NewFuselage.VisualProperties.ShowSurface = True
            NewFuselage.VisualProperties.ShowMesh = True

            Objects.Add(NewFuselage)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds a jet engine
        '==========================================================================================
        Public Sub AddJetEngine()

            Dim Engine As New JetEngine
            Engine.Name = String.Format("Jet engine - {0}", Objects.Count)
            Engine.GenerateMesh()
            Engine.IncludeInCalculation = True
            Engine.VisualProperties.ShowColormap = False
            Engine.VisualProperties.ShowLoadVectors = False
            Engine.VisualProperties.ShowNodes = False
            Engine.VisualProperties.ShowVelocityVectors = False
            Engine.VisualProperties.ShowPrimitives = False
            Engine.VisualProperties.ShowSurface = True
            Engine.VisualProperties.ShowMesh = True

            Objects.Add(Engine)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds a jet engine
        '==========================================================================================
        Public Sub AddPropeller()

            Dim Propeller As New Propeller
            Propeller.Name = String.Format("Propeller - {0}", Objects.Count)
            Propeller.IncludeInCalculation = True
            Propeller.VisualProperties.ShowColormap = False
            Propeller.VisualProperties.ShowLoadVectors = False
            Propeller.VisualProperties.ShowNodes = False
            Propeller.VisualProperties.ShowVelocityVectors = False
            Propeller.VisualProperties.ShowPrimitives = False
            Propeller.VisualProperties.ShowSurface = True
            Propeller.VisualProperties.ShowMesh = True

            Objects.Add(Propeller)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds an imported surface and sets it as current.
        '==========================================================================================
        Public Sub AddImportedSurface()

            Dim NewImportedSurface = New ImportedSurface
            NewImportedSurface.Name = "Imported mesh"
            Objects.Add(NewImportedSurface)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Generates a copy of the current lifting surface with the option symmetric in true
        '==========================================================================================
        Public Sub CloneObject(ByVal Index As Integer)

            If Index > 0 And Index <= Objects.Count Then
                Dim newSurface As Surface = Objects(Index).Clone
                If newSurface IsNot Nothing Then
                    Objects.Add(newSurface)
                End If
            End If

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

#Region " IO "

        '==========================================================================================
        '
        '==========================================================================================
        Public Sub ReadFromXML(ByRef Stream As XmlReader)

            Stream.Read()

            Name = Stream("Name")

            While Stream.Read()

                Select Case Stream.Name

                    Case "PolarDataBase"

                        PolarDataBase.ReadFromXML(Stream.ReadSubtree())

                    Case "CamberLinesDatabase"

                        CamberLinesDatabase.ReadFromXML(Stream.ReadSubtree())

                    Case "ModelProperties"

                        Dim SubReader As XmlReader = Stream.ReadSubtree()

                        SubReader.Read()

                        While SubReader.Read

                            If SubReader.NodeType = XmlNodeType.Element Then

                                If SubReader.Name = "LiftingSurface" Then

                                    Dim LiftingSurface As New LiftingSurface()

                                    LiftingSurface.ReadFromXML(SubReader.ReadSubtree)

                                    For I = 0 To LiftingSurface.WingRegions.Count - 1

                                        Dim Region As WingRegion = LiftingSurface.WingRegions(I)

                                        If Not Region.PolarId.Equals(Guid.Empty) Then

                                            Region.PolarFamiliy = PolarDataBase.GetFamilyFromId(Region.PolarId)

                                        End If

                                    Next

                                    Objects.Add(LiftingSurface)

                                ElseIf SubReader.Name = "ExtrudedSurface" Then

                                    Dim ExtrudedSurface As New Fuselage

                                    ExtrudedSurface.ReadFromXML(Stream.ReadSubtree)

                                    Objects.Add(ExtrudedSurface)

                                ElseIf SubReader.Name = "JetEngine" Then

                                    Dim JetEngine As New JetEngine

                                    JetEngine.ReadFromXML(Stream.ReadSubtree)

                                    Objects.Add(JetEngine)

                                ElseIf SubReader.Name = "Propeller" Then

                                    Dim Propeller As New Propeller()

                                    Propeller.ReadFromXML(SubReader.ReadSubtree)

                                    If Not Propeller.PolarId.Equals(Guid.Empty) Then

                                        Propeller.PolarFamiliy = PolarDataBase.GetFamilyFromId(Propeller.PolarId)

                                    End If

                                    Objects.Add(Propeller)

                                ElseIf SubReader.Name = "ImportedSurface" Then

                                    Dim Import As New ImportedSurface

                                    Import.ReadFromXML(Stream.ReadSubtree)

                                    Objects.Add(Import)

                                End If

                            End If


                        End While

                        SubReader.Close()

                End Select

            End While

            Stream.Close()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        '
        '==========================================================================================
        Public Sub WriteToXML(ByRef Stream As XmlWriter)

            Stream.WriteAttributeString("Name", Name)

            Stream.WriteStartElement("PolarDataBase")
            PolarDataBase.WriteToXML(Stream)
            Stream.WriteEndElement()

            Stream.WriteStartElement("CamberLinesDatabase")
            CamberLinesDatabase.WriteToXML(Stream)
            Stream.WriteEndElement()

            Stream.WriteStartElement("ModelProperties")

            For i = 0 To Objects.Count - 1

                If TypeOf Objects(i) Is LiftingSurface Then

                    Stream.WriteStartElement("LiftingSurface")

                    Objects(i).WriteToXML(Stream)

                    Stream.WriteEndElement()

                End If

                If TypeOf Objects(i) Is Fuselage Then

                    Stream.WriteStartElement("ExtrudedSurface")

                    Objects(i).WriteToXML(Stream)

                    Stream.WriteEndElement()

                End If

                If TypeOf Objects(i) Is JetEngine Then

                    Stream.WriteStartElement("JetEngine")

                    Objects(i).WriteToXML(Stream)

                    Stream.WriteEndElement()

                End If

                If TypeOf Objects(i) Is Propeller Then

                    Stream.WriteStartElement("Propeller")

                    Objects(i).WriteToXML(Stream)

                    Stream.WriteEndElement()

                End If

                If TypeOf Objects(i) Is ImportedSurface Then

                    Stream.WriteStartElement("ImportedSurface")

                    Objects(i).WriteToXML(Stream)

                    Stream.WriteEndElement()

                End If

            Next

            Stream.WriteEndElement()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Returns the global inertia of the model
        '==========================================================================================
        Public Function GetGlobalInertia() As InertialProperties

            Dim Inertia As InertialProperties

            Inertia.SetToZero()

            For Each Surface In Objects

                Dim LocalInertia As InertialProperties = Surface.Inertia

                ' Transform to global coordinates
                '-------------------------------------

                LocalInertia.Transform(Surface.Position, Surface.LocalDirections)

                ' Take into account complementary part
                ' in lifting surfaces
                '-------------------------------------

                If TypeOf Surface Is LiftingSurface Then

                    Dim Wing As LiftingSurface = Surface

                    If Wing.Symmetric Then

                        Dim ComplementaryInertia As InertialProperties = LocalInertia

                        ComplementaryInertia.Ixy *= -1.0
                        ComplementaryInertia.Iyz *= -1.0
                        ComplementaryInertia.Ycg *= -1.0

                        LocalInertia += ComplementaryInertia

                    End If

                End If

                ' Add to the total
                '-------------------------------------

                Inertia += LocalInertia

            Next

            Return Inertia

        End Function

#End Region

    End Class

End Namespace

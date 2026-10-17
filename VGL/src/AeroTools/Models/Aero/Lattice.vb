'#############################################################################
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

'' Standard .NET dependencies
'-----------------------------------------------------------------------------
Imports System.IO

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports VGL.AeroTools.Models.Aero.Components

'#############################################################################
' Unit: Lattice
'
' This unit provides the declaration of a generic lattice.
' Lattices are an interconnected network of vortex rings and serve as
' solid surface or wake.
'#############################################################################
Namespace AeroTools.Models.Aero

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Base clas for modelling lattices.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Lattice

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Name As String
        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Nodes As New List(Of Node)
        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public VortexRings As New List(Of VortexRing)
        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Vortices As New List(Of Vortex)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The lattice airloads
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property AirLoads As New AirLoads



        '==========================================================================================
        ' Adds a node to the lattice with an specific local index
        '==========================================================================================
        Public Overloads Sub AddNode(ByVal Position As Vector3, ByVal LocalIndex As Integer)
            Dim NodeToAdd As New Node(LocalIndex, Position) ' < Solucionar! si la estela se corta el numero cambia! (controlar si esta bien)
            Nodes.Add(NodeToAdd)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds a node to the lattice with a local index equal to the current number of nodes
        '==========================================================================================
        Public Overloads Sub AddNode(ByVal Position As Vector3)
            Dim NodeToAdd As New Node(Nodes.Count, Position) ' < Solucionar! si la estela se corta el numero cambia! (controlar si esta bien)
            Nodes.Add(NodeToAdd)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds a vortex ring to the lattice
        '==========================================================================================
        Public Sub AddVortexRing3(ByVal N1 As Integer, ByVal N2 As Integer, ByVal N3 As Integer, ByVal Reversed As Boolean, ByVal Slender As Boolean)
            Dim VortexRingToAdd As New VortexRing3(Nodes(N1), Nodes(N2), Nodes(N3), VortexRings.Count, Reversed, Slender)
            VortexRings.Add(VortexRingToAdd)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds a vortex ring to the lattice
        '==========================================================================================
        Public Sub AddVortexRing4(ByVal N1 As Integer, ByVal N2 As Integer, ByVal N3 As Integer, ByVal N4 As Integer, ByVal Reversed As Boolean, ByVal Slender As Boolean)
            Dim VortexRingToAdd As New VortexRing4(Nodes(N1), Nodes(N2), Nodes(N3), Nodes(N4), VortexRings.Count, Reversed, Slender)
            VortexRings.Add(VortexRingToAdd)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reasigns velocity at each nodal point.
        '==========================================================================================
        Public Overridable Overloads Sub ClearVelocity(ByVal Velocity As Vector3)

            For Each Node In Nodes

                Node.Velocity.Assign(Velocity)

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Computed velocity induced by this lattice at Point.
        '==========================================================================================
        Public Sub AddInducedVelocity(ByRef Velocity As Vector3, ByVal Point As Vector3, ByVal CutOff As Double)

            ' Add velocity associated to doublets (using the associated vortices)

            For Each Vortex In Vortices

                Vortex.AddInducedVelocity(Velocity, Point, CutOff, True)

            Next

            ' Add velocity associated with sources

            For Each Ring In VortexRings

                If Not Ring.IsSlender Then

                    Ring.AddSourceVelocityInfluence(Velocity, Point)

                End If

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Finds all vortices associated to this lattice.
        '==========================================================================================
        Public Sub PopulateVortices()

            Vortices.Clear()

            ' Scan each of the four vortices and, if they have not been added, add them to the list:

            Dim Order4(3, 1) As Integer

            Order4(0, 0) = 1
            Order4(0, 1) = 2

            Order4(1, 0) = 2
            Order4(1, 1) = 3

            Order4(2, 0) = 3
            Order4(2, 1) = 4

            Order4(3, 0) = 4
            Order4(3, 1) = 1

            Dim Order3(2, 1) As Integer

            Order3(0, 0) = 1
            Order3(0, 1) = 2

            Order3(1, 0) = 2
            Order3(1, 1) = 3

            Order3(2, 0) = 3
            Order3(2, 1) = 1

            Dim mi As Integer
            Dim mj As Integer

            Dim ni As Integer
            Dim nj As Integer

            For Each Ring In VortexRings

                Select Ring.Type

                    Case VortexRingType.VR4

                        For m = 0 To 3 ' For each local segment:

                            mi = Ring.Node(Order4(m, 0)).IndexL
                            mj = Ring.Node(Order4(m, 1)).IndexL

                            Dim Foud As Boolean = False

                            For Each Vortex In Vortices

                                ni = Vortex.Node1.IndexL
                                nj = Vortex.Node2.IndexL

                                ' If this segments already exist in the stack, there must be an adjacent ring to add:

                                If (mi = ni And mj = nj) Then
                                    For i = 0 To 2
                                        If IsNothing(Vortex.Rings(i)) Then
                                            Vortex.Rings(i) = Ring
                                            Vortex.Sence(i) = 1
                                            Foud = True
                                            Exit For
                                        End If
                                    Next
                                    Exit For
                                ElseIf (mi = nj And mj = ni) Then
                                    For i = 0 To 2
                                        If IsNothing(Vortex.Rings(i)) Then
                                            Vortex.Rings(i) = Ring
                                            Vortex.Sence(i) = -1
                                            Foud = True
                                            Exit For
                                        End If
                                    Next
                                    Exit For
                                End If

                            Next

                            If Not Foud Then

                                ' If it came up to here, the segment is new.

                                Dim Vortex As New Vortex
                                Vortex.Node1 = Nodes(mi)
                                Vortex.Node2 = Nodes(mj)
                                Vortex.Rings(0) = Ring
                                Vortex.Sence(0) = 1
                                Vortices.Add(Vortex)

                            End If

                        Next

                    Case VortexRingType.VR3

                        For m = 0 To 2 ' For each local segment:

                            mi = Ring.Node(Order3(m, 0)).IndexL
                            mj = Ring.Node(Order3(m, 1)).IndexL

                            Dim Foud As Boolean = False

                            For Each Vortex In Vortices

                                ni = Vortex.Node1.IndexL
                                nj = Vortex.Node2.IndexL

                                ' If this segments already exist in the stack, there must be an adjacent ring to add:

                                If (mi = ni And mj = nj) Then
                                    For i = 0 To 2
                                        If IsNothing(Vortex.Rings(i)) Then
                                            Vortex.Rings(i) = Ring
                                            Vortex.Sence(i) = 1
                                            Foud = True
                                            Exit For
                                        End If
                                    Next
                                    Exit For
                                ElseIf (mi = nj And mj = ni) Then
                                    For i = 0 To 2
                                        If IsNothing(Vortex.Rings(i)) Then
                                            Vortex.Rings(i) = Ring
                                            Vortex.Sence(i) = -1
                                            Foud = True
                                            Exit For
                                        End If
                                    Next
                                    Exit For
                                End If

                            Next

                            If Not Foud Then

                                ' If it came up to here, the segment is new.

                                Dim Vortex As New Vortex
                                Vortex.Node1 = Nodes(mi)
                                Vortex.Node2 = Nodes(mj)
                                Vortex.Rings(0) = Ring
                                Vortex.Sence(0) = 1
                                Vortices.Add(Vortex)

                            End If

                        Next

                End Select

            Next

        End Sub
        '------------------------------------------------------------------------------------------


#Region " Geometric operations "


        '==========================================================================================
        ' Calculate vortex rings geometric parameters (control point, normal vector)
        '==========================================================================================
        Public Sub CalculateLatticeParameters()

            For Each Ring In VortexRings
                Ring.CalculateGeometricEntities()
            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Rotates all nodal poins as specified in Orientation about the origin {0,0,0}
        '==========================================================================================
        Public Sub Rotate(ByVal Orientation As OrientationAngles)

            Dim M As New MathTools.Algebra.CustomMatrices.RotationMatrix
            M.Generate(Orientation)

            For Each Node In Nodes

                Node.Position.Rotate(M)

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Rotates all nodal points as specified in Orientation about an specific point.
        '==========================================================================================
        Public Sub RotateAbout(ByVal Orientation As OrientationAngles, ByVal Point As Vector3)

            Dim M As New MathTools.Algebra.CustomMatrices.RotationMatrix
            M.Generate(Orientation)

            For Each Node In Nodes

                Node.Position.Substract(Point)
                Node.Position.Rotate(M)
                Node.Position.Add(Point)

            Next

        End Sub
        '------------------------------------------------------------------------------------------


#End Region

#Region "I/O"


        '==========================================================================================
        ' Writes the lattice nodes, rings and vortices into the binary file.
        '==========================================================================================
        Public Overridable Sub WriteBinary(ByRef Stream As BinaryWriter, Optional ByVal NodalVelocity As Boolean = False, Optional ByVal ReferencePosition As Boolean = False)

            Stream.Write(NodalVelocity)
            Stream.Write(ReferencePosition)

            Dim lb As Integer = Nodes.First.IndexL ' Write the lower bound

            Stream.Write(lb)

            ' Nodes
            '-------------------------------------------------------

            Stream.Write(Nodes.Count)

            For Each Node In Nodes

                Stream.Write(Node.Position.X)
                Stream.Write(Node.Position.Y)
                Stream.Write(Node.Position.Z)

                If NodalVelocity Then

                    Stream.Write(Node.Velocity.X)
                    Stream.Write(Node.Velocity.Y)
                    Stream.Write(Node.Velocity.Z)

                End If

                If ReferencePosition Then

                    If Node.OriginalPosition Is Nothing Then
                        Stream.Write(Node.Position.X)
                        Stream.Write(Node.Position.Y)
                        Stream.Write(Node.Position.Z)
                    Else
                        Stream.Write(Node.OriginalPosition.X)
                        Stream.Write(Node.OriginalPosition.Y)
                        Stream.Write(Node.OriginalPosition.Z)
                    End If

                    If Node.Displacement Is Nothing Then
                        Stream.Write(0.0#)
                        Stream.Write(0.0#)
                        Stream.Write(0.0#)
                    Else
                        Stream.Write(Node.Displacement.X)
                        Stream.Write(Node.Displacement.Y)
                        Stream.Write(Node.Displacement.Z)
                    End If

                End If

            Next

            ' Rings
            '-------------------------------------------------------

            Stream.Write(VortexRings.Count)

            Dim IndexL As Integer = 0

            For Each Ring In VortexRings

                Stream.Write(Ring.Type)
                Stream.Write(Ring.IsSlender)
                Stream.Write(Ring.Reversed)

                IndexL = Ring.Node(1).IndexL - lb  ' < Indices have been corrected in case wake has been cut
                Stream.Write(IndexL)

                IndexL = Ring.Node(2).IndexL - lb
                Stream.Write(IndexL)

                IndexL = Ring.Node(3).IndexL - lb
                Stream.Write(IndexL)

                If Ring.Type = VortexRingType.VR4 Then

                    IndexL = Ring.Node(4).IndexL - lb
                    Stream.Write(IndexL)

                End If

                Stream.Write(Ring.G)

            Next

            ' Vortices
            '-------------------------------------------------------

            Stream.Write(Vortices.Count)

            For Each Vortex In Vortices

                Stream.Write(Vortex.Node1.IndexL)
                Stream.Write(Vortex.Node2.IndexL)
                Stream.Write(Vortex.G)

                For i = 0 to 1
                    If (IsNothing(Vortex.Rings(i))) Then
                        Stream.Write(-1)
                    Else
                        Stream.Write(Vortex.Rings(i).IndexL)
                    End If
                    Stream.Write(Vortex.Sence(i))
                Next

                Stream.Write(Vortex.Streamwise)

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the lattice nodes, rings and vortices from the binary file written by WriteBinary.
        '==========================================================================================
        Public Overridable Sub ReadBinary(ByRef Stream As BinaryReader)

            Nodes.Clear()
            VortexRings.Clear()

            Dim Position As New Vector3

            Dim NodalVelocity As Boolean = Stream.ReadBoolean
            Dim ReferenceAndDisplacement As Boolean = Stream.ReadBoolean

            Dim lb As Integer = Stream.ReadInt32

            Dim n As Integer = Stream.ReadInt32

            For i = 0 To n - 1

                Position.X = Stream.ReadDouble()
                Position.Y = Stream.ReadDouble()
                Position.Z = Stream.ReadDouble()
                Nodes.Add(New Node(i, Position))

                If NodalVelocity Then

                    Nodes(i).Velocity.X = Stream.ReadDouble()
                    Nodes(i).Velocity.Y = Stream.ReadDouble()
                    Nodes(i).Velocity.Z = Stream.ReadDouble()

                End If

                If ReferenceAndDisplacement Then

                    Nodes(i).OriginalPosition = New Vector3(Stream.ReadDouble(), Stream.ReadDouble(), Stream.ReadDouble())

                    Nodes(i).Displacement = New Vector3(Stream.ReadDouble(), Stream.ReadDouble(), Stream.ReadDouble())

                End If

            Next

            Dim NodeIndex(3) As Integer

            n = Stream.ReadInt32

            For i = 0 To n - 1

                Dim vrType As VortexRingType = Stream.ReadByte
                Dim vrSlender As Boolean = Stream.ReadBoolean
                Dim vrReversed As Boolean = Stream.ReadBoolean

                NodeIndex(0) = Stream.ReadInt32
                NodeIndex(1) = Stream.ReadInt32
                NodeIndex(2) = Stream.ReadInt32

                If vrType = VortexRingType.VR4 Then

                    NodeIndex(3) = Stream.ReadInt32
                    VortexRings.Add(New VortexRing4(Nodes(NodeIndex(0)), Nodes(NodeIndex(1)), Nodes(NodeIndex(2)), Nodes(NodeIndex(3)), Stream.ReadDouble, i, vrReversed, vrSlender))

                Else

                    VortexRings.Add(New VortexRing3(Nodes(NodeIndex(0)), Nodes(NodeIndex(1)), Nodes(NodeIndex(2)), Stream.ReadDouble, i, vrReversed, vrSlender))

                End If

            Next

            n = Stream.ReadInt32

            For i = 0 To n - 1

                Dim vortex As New Vortex

                NodeIndex(0) = Stream.ReadInt32 - lb
                NodeIndex(1) = Stream.ReadInt32 - lb

                vortex.Node1 = Nodes(NodeIndex(0))
                vortex.Node2 = Nodes(NodeIndex(1))
                vortex.G = Stream.ReadDouble

                NodeIndex(0) = Stream.ReadInt32
                If NodeIndex(0) >= 0 And NodeIndex(0) < Nodes.Count Then
                    vortex.Rings(0) = VortexRings(NodeIndex(0))
                End If
                vortex.Sence(0) = Stream.ReadSByte

                NodeIndex(1) = Stream.ReadInt32
                If NodeIndex(1) >= 0 And NodeIndex(1) < Nodes.Count Then
                    vortex.Rings(1) = VortexRings(NodeIndex(1))
                End If
                vortex.Sence(1) = Stream.ReadSByte

                vortex.Streamwise = Stream.ReadBoolean

                Vortices.Add(vortex)

            Next

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------

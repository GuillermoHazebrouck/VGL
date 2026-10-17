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
Imports VGL.AeroTools.Models.Aero.Components
Imports VGL.MathTools.Algebra.EuclideanSpace

'#############################################################################
' Unit: BoundedLattice
'
' This unit provides a declaration of a bounded lattice.
' Bounded lattices are lattices that represent solid boundaries and can 
' contain edges from where wakes are shed.
'#############################################################################
Namespace AeroTools.Models.Aero

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a lattice bounded to a solid surface where several wakes might be convected.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class BoundedLattice

        Inherits Lattice

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Wakes As New List(Of Wake)


        '==========================================================================================
        '
        '==========================================================================================
        Public Sub New()
            Wakes = New List(Of Wake)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' This sub repositions the primitive points at the wake. It is used on the aeroelastic 
        ' analysis.
        '==========================================================================================
        Public Sub ReEstablishWakes()

            For Each Wake In Wakes

                Dim StartingIndex As Integer = Wake.Nodes.Count - Wake.Primitive.Nodes.Count  ' < Last local index

                For Each PrimNode In Wake.Primitive.Nodes

                    Wake.Nodes(StartingIndex).Position.Assign(Nodes(PrimNode).Position)

                    StartingIndex += 1

                Next

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reasigns velocity at each control point.
        '==========================================================================================
        Public Overloads Overrides Sub ClearVelocity(ByVal Velocity As Vector3)

            For Each VortexRing In VortexRings
                VortexRing.VelocityW.Assign(Velocity)
            Next

        End Sub
        '------------------------------------------------------------------------------------------


#Region " Rings on wakes "


        '==========================================================================================
        '
        '==========================================================================================
        Public Sub InitializeWakeRings()

            For Each Wake As Wake In Wakes

                For Each PrimNode In Wake.Primitive.Nodes

                    Wake.AddNode(Nodes(PrimNode).Position)

                Next

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Convect all wakes on the primiteve edges of the bound lattice and adds new nodes and 
        ' panels at primitive positions.
        ' If RemoveOldest is marked as true, oldest wake panels will be removed.
        '==========================================================================================
        Public Sub PopulateWakeRings(Dt As Double,
                                     TimeStep As Integer,
                                     ExtendWakes As Boolean,
                                     Extension As Vector3)

            Dim N1 As Integer
            Dim N2 As Integer
            Dim N3 As Integer
            Dim N4 As Integer

            For Each Wake As Wake In Wakes

                Wake.Convect(Dt)

                ' Add new nodes:

                Dim NodesCount As Integer = Wake.Nodes.Count
                Dim PrimitiveNodesCount As Integer = Wake.Primitive.Nodes.Count
                Dim PrimitiveRingsCount As Integer = Wake.Primitive.Rings.Count
                Dim LocalIndex As Integer = Wake.Nodes(Wake.Nodes.Count - 1).IndexL ' < Last local index

                For Each PrimNode In Wake.Primitive.Nodes

                    LocalIndex += 1
                    Wake.AddNode(Nodes(PrimNode).Position, LocalIndex)

                    ' Index has been corrected to follow consecutive order. 
                    ' If the wake has been cut, index wont start from 0. Nevertheless it will always follow a consecutive order.

                Next

                ' Add new vortex rings:

                NodesCount = Wake.Nodes.Count
                N4 = NodesCount - 2 * PrimitiveNodesCount
                N3 = NodesCount - PrimitiveNodesCount
                N2 = N3 + 1
                N1 = N4 + 1

                ' N4 ## N1 < convected nodes
                ' #     #
                ' #     #
                ' N3 ## N2 < primitive edge >

                For i = 0 To PrimitiveRingsCount - 1

                    ' Add wake ring i:

                    Wake.AddVortexRing4(N1 + i, N2 + i, N3 + i, N4 + i, VortexRings(Wake.Primitive.Rings(i)).G)

                Next

                ' Remove old rings, nodes and vortices

                If TimeStep > Wake.CuttingStep Then

                    Wake.VortexRings.RemoveRange(0, PrimitiveRingsCount)
                    Wake.Nodes.RemoveRange(0, PrimitiveNodesCount)

                    ' Extend the trailing part of the wake if requested

                    If ExtendWakes AndAlso TimeStep > Wake.CuttingStep + 5 Then

                        Wake.Extend(Extension)

                    End If

                End If

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Convect all wakes on the primiteve edges of the bound lattice and adds new nodes and 
        ' panels at primitive positions.
        ' If RemoveOldest is marked as true, oldest wake panels will be removed.
        '==========================================================================================
        Public Sub PopulateWakeRingsAndVortices(Dt As Double,
                                                TimeStep As Integer,
                                                ExtendWakes As Boolean,
                                                Extension As Vector3)

            Dim N1 As Integer
            Dim N2 As Integer
            Dim N3 As Integer
            Dim N4 As Integer

            For Each Wake As Wake In Wakes

                Wake.Convect(Dt)

                ' Add new nodes:

                Dim NodesCount As Integer = Wake.Nodes.Count
                Dim VorticesCount As Integer = Wake.Vortices.Count
                Dim PrimitiveNodesCount As Integer = Wake.Primitive.Nodes.Count
                Dim PrimitiveRingsCount As Integer = Wake.Primitive.Rings.Count
                Dim LocalIndex As Integer = Wake.Nodes(NodesCount - 1).IndexL ' < Last local index

                ' Add nodes:

                For i = 0 To PrimitiveNodesCount - 1

                    LocalIndex += 1
                    Wake.AddNode(Nodes(Wake.Primitive.Nodes(i)).Position, LocalIndex)

                Next

                ' Add streamwise vortices:

                For i = 0 To PrimitiveNodesCount - 1

                    Dim StreamWiseVortex As New Vortex

                    StreamWiseVortex.Node1 = Wake.Nodes(NodesCount - PrimitiveNodesCount + i)
                    StreamWiseVortex.Node2 = Wake.Nodes(NodesCount + i)
                    StreamWiseVortex.Streamwise = True
                    Wake.Vortices.Add(StreamWiseVortex)

                Next

                ' Add transverse vortices:

                For i = 1 To PrimitiveNodesCount - 1

                    Dim TransverseVortex As New Vortex

                    TransverseVortex.Node1 = Wake.Nodes(NodesCount + i - 1)
                    TransverseVortex.Node2 = Wake.Nodes(NodesCount + i)

                    Wake.Vortices.Add(TransverseVortex)

                Next

                ' Assign circulation

                For i = 0 To PrimitiveRingsCount - 1

                    Dim G As Double = VortexRings(Wake.Primitive.Rings(i)).G

                    Wake.Vortices(VorticesCount - PrimitiveRingsCount + i).G += G
                    Wake.Vortices(VorticesCount + i).G -= G
                    Wake.Vortices(VorticesCount + i + 1).G += G
                    Wake.Vortices(VorticesCount + PrimitiveNodesCount + i).G -= G

                Next

                ' If TameInnerCirculation Then Wake.Vortices(VorticesCount).G = 0

                ' Remove old vortices and nodes

                If TimeStep > Wake.CuttingStep Then

                    Wake.Nodes.RemoveRange(0, PrimitiveNodesCount)
                    Wake.Vortices.RemoveRange(0, PrimitiveNodesCount + PrimitiveRingsCount)

                End If

                ' Add new vortex rings:

                NodesCount = Wake.Nodes.Count
                N4 = NodesCount - 2 * PrimitiveNodesCount
                N3 = NodesCount - PrimitiveNodesCount
                N2 = N3 + 1
                N1 = N4 + 1

                ' N4 ## N1 < convected nodes
                ' #     #
                ' #     #
                ' N3 ## N2 < primitive edge >

                Dim nr As Integer = Wake.VortexRings.Count

                For i = 0 To PrimitiveRingsCount - 1

                    ' Add wake ring i:

                    Wake.AddVortexRing4(N1 + i, N2 + i, N3 + i, N4 + i, VortexRings(Wake.Primitive.Rings(i)).G)

                Next

                'If TameInnerCirculation Then Wake.VortexRings(nr).G = 0.0#

                ' Remove old rings, nodes and vortices

                If TimeStep > Wake.CuttingStep Then

                    Wake.VortexRings.RemoveRange(0, PrimitiveRingsCount)

                    ' Extend the trailing part of the wake if requested

                    If ExtendWakes AndAlso TimeStep > Wake.CuttingStep + 5 Then

                        Wake.Extend(Extension)

                    End If

                End If

            Next

        End Sub
        '------------------------------------------------------------------------------------------



#End Region

#Region " Vortices on wakes "


        '==========================================================================================
        '
        '==========================================================================================
        Public Sub InitializeWakeVortices()

            For Each Wake As Wake In Wakes

                For Each PrimNode In Wake.Primitive.Nodes

                    Wake.AddNode(Nodes(PrimNode).Position)

                Next

                For i = 1 To Wake.Primitive.Nodes.Count - 1

                    Dim Vortex As New Vortex
                    Vortex.Node1 = Wake.Nodes(i - 1)
                    Vortex.Node2 = Wake.Nodes(i)
                    Vortex.G = 0.0#
                    Wake.Vortices.Add(Vortex)

                Next

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Convect all wakes on the primiteve edges of the bound lattice and adds new nodes and
        ' panels at primitive positions.
        '==========================================================================================
        Public Sub PopulateWakeVortices(Dt As Double,
                                        TimeStep As Integer,
                                        ExtendWakes As Boolean,
                                        Extension As Vector3)

            For Each Wake As Wake In Wakes

                Wake.Convect(Dt)

                ' Add new nodes:

                Dim NodesCount As Integer = Wake.Nodes.Count
                Dim VorticesCount As Integer = Wake.Vortices.Count
                Dim PrimitiveNodesCount As Integer = Wake.Primitive.Nodes.Count
                Dim PrimitiveRingsCount As Integer = Wake.Primitive.Rings.Count
                Dim LocalIndex As Integer = Wake.Nodes(NodesCount - 1).IndexL ' < Last local index

                ' Add nodes:

                For i = 0 To PrimitiveNodesCount - 1

                    LocalIndex += 1
                    Wake.AddNode(Nodes(Wake.Primitive.Nodes(i)).Position, LocalIndex)

                Next

                ' Add streamwise vortices:

                For i = 0 To PrimitiveNodesCount - 1

                    Dim StreamWiseVortex As New Vortex

                    StreamWiseVortex.Node1 = Wake.Nodes(NodesCount - PrimitiveNodesCount + i)
                    StreamWiseVortex.Node2 = Wake.Nodes(NodesCount + i)
                    StreamWiseVortex.Streamwise = True
                    Wake.Vortices.Add(StreamWiseVortex)

                Next

                ' Add transverse vortices:

                For i = 1 To PrimitiveNodesCount - 1

                    Dim TransverseVortex As New Vortex

                    TransverseVortex.Node1 = Wake.Nodes(NodesCount + i - 1)
                    TransverseVortex.Node2 = Wake.Nodes(NodesCount + i)
                    Wake.Vortices.Add(TransverseVortex)

                Next

                ' Assign circulation

                For i = 0 To PrimitiveRingsCount - 1

                    Dim G As Double = VortexRings(Wake.Primitive.Rings(i)).G

                    Wake.Vortices(VorticesCount - PrimitiveRingsCount + i).G += G
                    Wake.Vortices(VorticesCount + i).G -= G
                    Wake.Vortices(VorticesCount + i + 1).G += G
                    Wake.Vortices(VorticesCount + PrimitiveNodesCount + i).G -= G

                Next

                ' Do this when there is a fuselage to avoid wake rollup next to the fuselage:

                If Wake.SupressInnerCircuation Then Wake.Vortices(VorticesCount).G = 0

                ' Remove old vortices and nodes

                If TimeStep > Wake.CuttingStep Then

                    Wake.Nodes.RemoveRange(0, PrimitiveNodesCount)
                    Wake.Vortices.RemoveRange(0, PrimitiveNodesCount + PrimitiveRingsCount)

                    ' Extend the trailing part of the wake if requested

                    If ExtendWakes AndAlso TimeStep > Wake.CuttingStep + 5 Then

                        Wake.Extend(Extension)

                    End If

                End If

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Stores the current position of the wake for later use.
        '==========================================================================================
        Public Sub CacheWakePosition()

            For Each Wake As Wake In Wakes
                Wake.CachePosition
            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reconvects the nodes having an original position.
        '==========================================================================================
        Public Sub ReconvectWakes(Dt)

            For Each Wake As Wake In Wakes
                Wake.ReConvect(Dt)
            Next

        End Sub
        '------------------------------------------------------------------------------------------



#End Region

#Region " Loads "

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Links to chordwise ring stripes used to calculate skin drag and local CL
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ChordWiseStripes As New List(Of ChorwiseStripe)



        '==========================================================================================
        ' Calculates pressure coeficient at each bounded vortex ring.
        '==========================================================================================
        Public Sub CalculatePressure(ByVal SquareVelocity As Double)

            For Each Ring In VortexRings

                Ring.CalculateCp(SquareVelocity)

            Next

        End Sub
        '------------------------------------------------------------------------------------------



#End Region

#Region " IO "

        '==========================================================================================
        ' Writes the lattice nodes, rings and vortices into the binary file.
        '==========================================================================================
        Public Overrides Sub WriteBinary(ByRef Stream As System.IO.BinaryWriter, Optional ByVal NodalVelocity As Boolean = False, Optional ByVal ReferencePosition As Boolean = False)

            MyBase.WriteBinary(Stream, NodalVelocity, True)

            ' Write vortex rings properties after geometry
            '---------------------------------------------

            For i = 0 To VortexRings.Count - 1
                Stream.Write(VortexRings(i).Cp)
                Stream.Write(VortexRings(i).DGdt)
                Stream.Write(VortexRings(i).VelocityT.X)
                Stream.Write(VortexRings(i).VelocityT.Y)
                Stream.Write(VortexRings(i).VelocityT.Z)
                Stream.Write(VortexRings(i).Cdi)
            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Writes the lattice nodes, rings and vortices into the binary file.
        '==========================================================================================
        Public Overloads Sub WriteBinary(ByVal FilePath As String, Optional ByVal WakesNodalVelocity As Boolean = False)

            Dim Stream As New BinaryWriter(New FileStream(FilePath, FileMode.Create))

            ' Base
            '--------------------------

            Me.WriteBinary(Stream, True)

            ' Wakes
            '--------------------------

            Stream.Write(Wakes.Count)

            For i = 0 To Wakes.Count - 1
                Wakes(i).WriteBinary(Stream, WakesNodalVelocity)
            Next

            ' Chordwise links
            '--------------------------

            Stream.Write(ChordWiseStripes.Count)

            For i = 0 To ChordWiseStripes.Count - 1
                ChordWiseStripes(i).WriteBinary(Stream)
            Next

            ' Air-loads
            '--------------------------

            AirLoads.WriteBinary(Stream)

            Stream.Close()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the lattice nodes, rings and vortices from the binary file written by WriteBinary.
        '==========================================================================================
        Public Overrides Sub ReadBinary(ByRef Stream As System.IO.BinaryReader)

            MyBase.ReadBinary(Stream)

            For i = 0 To VortexRings.Count - 1
                VortexRings(i).Cp = Stream.ReadDouble
                VortexRings(i).DGdt = Stream.ReadDouble
                VortexRings(i).VelocityT.X = Stream.ReadDouble
                VortexRings(i).VelocityT.Y = Stream.ReadDouble
                VortexRings(i).VelocityT.Z = Stream.ReadDouble
                VortexRings(i).Cdi = Stream.ReadDouble
            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the lattice nodes, rings and vortices from the binary file written by WriteBinary.
        '==========================================================================================
        Public Overloads Sub ReadBinary(ByVal FilePath As String, Optional ByVal Polars As PolarDatabase = Nothing, Optional ByVal WithWakes As Boolean = True)

            If File.Exists(FilePath) Then

                Dim Stream As New BinaryReader(New FileStream(FilePath, FileMode.Open))

                ' Base
                '--------------------------

                Me.ReadBinary(Stream)

                CalculateLatticeParameters()

                ' Wakes
                '--------------------------

                Wakes.Clear()

                If WithWakes Then

                    For i = 0 To Stream.ReadInt32 - 1
                        Wakes.Add(New Wake)
                        Wakes(i).ReadBinary(Stream)
                    Next

                End If

                ' Chordwise links
                '--------------------------

                Try
                    For i = 0 To Stream.ReadInt32 - 1
                        ChordWiseStripes.Add(New ChorwiseStripe)
                        ChordWiseStripes(i).ReadBinary(Stream, VortexRings, Polars)
                    Next
                Catch
                    ' No links were found
                    ChordWiseStripes.Clear()
                End Try

                ' Air-loads
                '--------------------------

                AirLoads.ReadBinary(Stream)

                Stream.Close()

            End If

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class
    '---------------------------------------------------------------------------------------------- 

End Namespace
'--------------------------------------------------------------------------------------------------
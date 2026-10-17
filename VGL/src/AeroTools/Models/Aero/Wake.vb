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
Imports System.Threading.Tasks

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.AeroTools.Models.Aero.Components
Imports VGL.MathTools.Algebra.EuclideanSpace

'#############################################################################
' Unit: Wake
'
' This unit provides the declaration of a generic wake.
' The wakes are free lattices that move with the stream.
'#############################################################################
Namespace AeroTools.Models.Aero

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a free lattice.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Wake

        Inherits Lattice

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Contains information about the sheding edge.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Primitive As New Primitive

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates when the wake has to be trimmed.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CuttingStep As Integer = 100

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the inner circulation must be supressed in order to model
        ' an anchor line.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SupressInnerCircuation As Boolean = False

        '==========================================================================================
        ' Convects the wake using the local nodal velocity
        ' Dt is the time interval
        '==========================================================================================
        Public Sub Convect(ByVal Dt As Double)

            Parallel.ForEach(Nodes,
                             Sub(Node As Node)
                                 Node.Position.X += Dt * Node.Velocity.X
                                 Node.Position.Y += Dt * Node.Velocity.Y
                                 Node.Position.Z += Dt * Node.Velocity.Z
                             End Sub)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Caches the position of the nodes in the "OriginalPosition".
        '==========================================================================================
        Public Sub CachePosition()

            For Each Node In Nodes
                If Node.OriginalPosition Is Nothing Then
                    Node.OriginalPosition = New Vector3(Node.Position)
                Else
                    Node.OriginalPosition.X = Node.Position.X
                    Node.OriginalPosition.Y = Node.Position.Y
                    Node.OriginalPosition.Z = Node.Position.Z
                End If
            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Convects the wake using the local nodal velocity
        '==========================================================================================
        Public Sub ReConvect(ByVal Dt As Double)

            Parallel.ForEach(Nodes,
                             Sub(Node As Node)
                                 If Node.OriginalPosition IsNot Nothing Then
                                     Node.Position.X = Node.OriginalPosition.X + Dt * Node.Velocity.X
                                     Node.Position.Y = Node.OriginalPosition.Y + Dt * Node.Velocity.Y
                                     Node.Position.Z = Node.OriginalPosition.Z + Dt * Node.Velocity.Z
                                 End If
                             End Sub)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Extends the trailing part of the wake
        ' Vector is the vector used to extend the wake
        '==========================================================================================
        Public Sub Extend(ByVal Vector As Vector3)

            For i = 0 To Primitive.Nodes.Count - 1
                Nodes(i).Position.X += Vector.X
                Nodes(i).Position.Y += Vector.Y
                Nodes(i).Position.Z += Vector.Z
            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        '
        '==========================================================================================
        Public Overloads Sub AddVortexRing4(ByVal N1 As Integer, ByVal N2 As Integer, ByVal N3 As Integer, ByVal N4 As Integer, ByVal G As Double)

            Dim VortexRing As New VortexRing4(Nodes(N1), Nodes(N2), Nodes(N3), Nodes(N4), G, VortexRings.Count, False, True)

            VortexRings.Add(VortexRing)

        End Sub
        '------------------------------------------------------------------------------------------


#Region "I/O"


        '==========================================================================================
        ' Writes the lattice nodes, rings and vortices into the binary file.
        '==========================================================================================
        Public Overrides Sub WriteBinary(ByRef Stream As BinaryWriter, Optional ByVal NodalVelocity As Boolean = False, Optional ByVal ReferencePosition As Boolean = False)

            MyBase.WriteBinary(Stream, NodalVelocity)

            Stream.Write(Primitive.Nodes.Count)

            For i = 0 To Primitive.Nodes.Count - 1
                Stream.Write(Primitive.Nodes(i))
            Next

            Stream.Write(Primitive.Rings.Count)

            For i = 0 To Primitive.Rings.Count - 1
                Stream.Write(Primitive.Rings(i))
            Next

            Stream.Write(CuttingStep)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        '
        '==========================================================================================
        Public Overrides Sub ReadBinary(ByRef Stream As BinaryReader)

            MyBase.ReadBinary(Stream)

            Primitive.Nodes.Clear()
            Primitive.Rings.Clear()

            For i = 0 To Stream.ReadInt32 - 1
                Primitive.Nodes.Add(Stream.ReadInt32)
            Next

            For i = 0 To Stream.ReadInt32 - 1
                Primitive.Rings.Add(Stream.ReadInt32)
            Next

            CuttingStep = Stream.ReadInt32

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
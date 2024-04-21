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

'' Standard .NET dependencies
'-----------------------------------------------------------------------------
Imports System.IO

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports VGL.AeroTools.Models.Structural.Library
Imports VGL.AeroTools.Models.Aero.Components
Imports VGL.MathTools.Integration
Imports VGL.AeroTools.Models.Structural.Library.Nodes
Imports VGL.AeroTools.Models.Structural.Library.Elements

'#############################################################################
' Unit: Linker
'
' This unit is where the structural model is linked to the aerodynamic model.
' This is done by the introduction of kinematic and mechanic links.
'#############################################################################
Namespace AeroTools.Models.Structural

    ''' <summary>
    ''' Provides a kinematic link between structural and aerodynamic models with 6 DOFs
    ''' </summary>
    ''' <remarks>This class is able to convert the motion of an structural node to the nodes of a node-stripe</remarks>
    Public Class KinematicLink

        Private _LinkedStripe As List(Of Node)

        ''' <summary>
        ''' Collection of aerodynamic nodal points (deformed structure)
        ''' </summary>
        ''' <remarks></remarks>
        Public ReadOnly Property LinkedStripe As List(Of Node)
            Get
                Return _LinkedStripe
            End Get
        End Property

        Private _LinkedNode As StructuralNode

        ''' <summary>
        ''' Structural node which will transfer motion to the linked node-stripe
        ''' </summary>
        ''' <remarks></remarks>
        Public ReadOnly Property LinkedNode As StructuralNode
            Get
                Return _LinkedNode
            End Get
        End Property

        ''' <summary>
        ''' Creates a kinematic link
        ''' </summary>
        ''' <param name="LinkedNode">Structural node to be linked</param>
        ''' <remarks></remarks>
        Public Sub New(ByVal LinkedNode As StructuralNode, ByRef LinkedStripe As List(Of Node))

            _LinkedStripe = LinkedStripe
            _LinkedNode = LinkedNode

        End Sub

        Public Sub New(ByVal LinkedNode As StructuralNode)

            _LinkedStripe = New List(Of Node)
            _LinkedNode = LinkedNode

        End Sub

        ''' <summary>
        ''' Links a given aerodynamic node to the structural node associated to this link.
        ''' </summary>
        ''' <param name="Node">Aerodinamic node to be linked</param>
        ''' <remarks></remarks>
        Public Sub Link(ByRef Node As Node)

            ' Add reference to the node:

            _LinkedStripe.Add(Node)

            ' Save the current position as reference position:

            Node.OriginalPosition = New Vector3(Node.Position)

            ' Initialize nodal displacement

            Node.Displacement = New Vector3

        End Sub

        ''' <summary>
        ''' Transfer the motion from the structural model to the aerodynamic model
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub TransferMotion()

            Dim RX, RY, RZ As Double

            For Each Node As Node In _LinkedStripe

                ' Calculate relative position (on the undeformed state): <actually, this could be done once>

                RX = Node.OriginalPosition.X - _LinkedNode.Position.X
                RY = Node.OriginalPosition.Y - _LinkedNode.Position.Y
                RZ = Node.OriginalPosition.Z - _LinkedNode.Position.Z

                ' Calculate displacement:

                Node.Displacement.X = _LinkedNode.Displacement.Dx + _LinkedNode.Displacement.Ry * RZ - _LinkedNode.Displacement.Rz * RY
                Node.Displacement.Y = _LinkedNode.Displacement.Dy + _LinkedNode.Displacement.Rz * RX - _LinkedNode.Displacement.Rx * RZ
                Node.Displacement.Z = _LinkedNode.Displacement.Dz + _LinkedNode.Displacement.Rx * RY - _LinkedNode.Displacement.Ry * RX

                ' Calculate velocity:

                Node.Velocity.X = _LinkedNode.Velocity.Dx + _LinkedNode.Velocity.Ry * RZ - _LinkedNode.Velocity.Rz * RY
                Node.Velocity.Y = _LinkedNode.Velocity.Dy + _LinkedNode.Velocity.Rz * RX - _LinkedNode.Velocity.Rx * RZ
                Node.Velocity.Z = _LinkedNode.Velocity.Dz + _LinkedNode.Velocity.Rx * RY - _LinkedNode.Velocity.Ry * RX

                ' Calculate new (deformed) position:

                Node.Position.X = Node.OriginalPosition.X + Node.Displacement.X
                Node.Position.Y = Node.OriginalPosition.Y + Node.Displacement.Y
                Node.Position.Z = Node.OriginalPosition.Z + Node.Displacement.Z

            Next

        End Sub

    End Class

    ''' <summary>
    ''' Provides a mechanic link throug which aerodynamic loads are transfered to the structural model
    ''' </summary>
    ''' <remarks></remarks>
    Public Class MechanicLink

        ' This link calculates the local Cm, Cl and Cd and transfer them to both element nodes

        Private _LinkedStripe As List(Of VortexRing)

        ''' <summary>
        ''' Collection of vortex rings which will transfer loads to the structure
        ''' </summary>
        ''' <remarks></remarks>
        Public ReadOnly Property LinkedStripe As List(Of VortexRing)
            Get
                Return _LinkedStripe
            End Get
        End Property

        Private _LinkedElement As BeamElement

        ''' <summary>
        ''' Structural element which will recieve the loads from the panel-stripe
        ''' </summary>
        ''' <remarks></remarks>
        Public ReadOnly Property LinkedElement As BeamElement
            Get
                Return _LinkedElement
            End Get
        End Property

        Public Polar As IPolarCurve

        Public Sub New(ByVal LinkedElement As BeamElement, ByRef LinkedStripe As List(Of VortexRing))

            _LinkedElement = LinkedElement
            _LinkedStripe = LinkedStripe

        End Sub

        Public Sub New(ByVal LinkedElement As BeamElement)

            _LinkedElement = LinkedElement
            _LinkedStripe = New List(Of VortexRing)

        End Sub

        ''' <summary>
        ''' Links a vortex ring to the structural element associated to this link.
        ''' </summary>
        ''' <param name="Ring"></param>
        ''' <remarks></remarks>
        Public Sub Link(ByRef Ring As VortexRing)

            _LinkedStripe.Add(Ring)

        End Sub

        ''' <summary>
        ''' Transfer loads from the aerodynamic model to the structural model
        ''' TODO: this metod should take into account the missing leading edge pressure decay
        ''' by substracting the projection of the force in the stream velocity direction.
        ''' </summary>
        ''' <param name="Velocity">Reference velocity</param>
        ''' <remarks></remarks>
        Public Sub TransferLoads(ByVal Velocity As Vector3, ByVal Density As Double)

            'Dim L As New EVector3 ' Local lift
            'Dim D As New EVector3 ' Local drag (only parasitic currently included)
            Dim _Fx, _Fy, _Fz As Double ' Total force
            Dim _Lx, _Ly, _Lz As Double ' Local force
            Dim _Mx, _My, _Mz As Double ' Total moment about the element's middle point
            Dim _Rx, _Ry, _Rz As Double ' Relative position vector
            Dim P As Double
            Dim q As Double = 0.25 * Velocity.SquareEuclideanNorm * Density ' divided by 2 to split contribution on each structural node (to make calculation more efficient)

            ' Sum force and moment contribution from each vortex ring on the linked stripe:

            For Each Ring In _LinkedStripe

                P = Ring.Cp * Ring.Area * q ' Local jump of pressure

                _Lx = P * Ring.Normal.X
                _Ly = P * Ring.Normal.Y
                _Lz = P * Ring.Normal.Z

                _Fx += _Lx
                _Fy += _Ly
                _Fz += _Lz

                _Rx = Ring.ControlPoint.X - 0.5 * (LinkedElement.NodeA.Position.X + LinkedElement.NodeB.Position.X)
                _Ry = Ring.ControlPoint.Y - 0.5 * (LinkedElement.NodeA.Position.Y + LinkedElement.NodeB.Position.Y)
                _Rz = Ring.ControlPoint.Z - 0.5 * (LinkedElement.NodeA.Position.Z + LinkedElement.NodeB.Position.Z)

                _Mx += _Ry * _Lz - _Rz * _Ly
                _My += _Rz * _Lx - _Rx * _Lz
                _Mz += _Rx * _Ly - _Ry * _Lx

            Next

            ' Each node of the linked element receives the halft of the load: (0.5 factor has already been applied to the dynamic pressure)

            _LinkedElement.NodeA.Load.Px += _Fx
            _LinkedElement.NodeA.Load.Py += _Fy
            _LinkedElement.NodeA.Load.Pz += _Fz
            _LinkedElement.NodeA.Load.Tx += _Mx
            _LinkedElement.NodeA.Load.Ty += _My
            _LinkedElement.NodeA.Load.Tz += _Mz

            _LinkedElement.NodeB.Load.Px += _Fx
            _LinkedElement.NodeB.Load.Py += _Fy
            _LinkedElement.NodeB.Load.Pz += _Fz
            _LinkedElement.NodeB.Load.Tx += _Mx
            _LinkedElement.NodeB.Load.Ty += _My
            _LinkedElement.NodeB.Load.Tz += _Mz

        End Sub

        ''' <summary>
        ''' Sets the loads to cero
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub ClearLoads()
            _LinkedElement.NodeA.Load.Clear()
            _LinkedElement.NodeB.Load.Clear()
        End Sub

        ''' <summary>
        ''' Resets normal vectors and control points on linked rings
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub ReloadGeometricEntities()

            For Each Ring In _LinkedStripe

                Ring.CalculateGeometricEntities()

            Next

        End Sub

    End Class

    ''' <summary>
    ''' Simulates an aeroelastic interaction by linkin the structural model to the aerodynamic model
    ''' </summary>
    ''' <remarks></remarks>
    Public Class StructuralLink

        ''' <summary>
        ''' System structure
        ''' </summary>
        ''' <remarks></remarks>
        Public StructuralCore As StructuralCore

        ''' <summary>
        ''' Collection of kinematic links
        ''' </summary>
        ''' <remarks></remarks>
        Public KinematicLinks As List(Of KinematicLink)

        ''' <summary>
        ''' Collection of mechanic links
        ''' </summary>
        ''' <remarks></remarks>
        Public MechanicLinks As List(Of MechanicLink)

        ''' <summary>
        ''' Collection of modal coordinates for each time-step
        ''' </summary>
        ''' <remarks></remarks>
        Public ModalResponse As List(Of ModalCoordinates)

        ''' <summary>
        ''' List of Newmark inegrators for each mode.
        ''' </summary>
        ''' <remarks></remarks>
        Private NewmarkIntegrators As List(Of NewmarkIntegrator)

        Public Sub New()

            StructuralCore = New StructuralCore
            KinematicLinks = New List(Of KinematicLink)
            MechanicLinks = New List(Of MechanicLink)
            ModalResponse = New List(Of ModalCoordinates)

        End Sub

        ''' <summary>
        ''' Current time step
        ''' </summary>
        ''' <remarks></remarks>
        Private T As Integer = 0

        ''' <summary>
        ''' Time interval.
        ''' </summary>
        ''' <remarks></remarks>
        Private Dt As Double

        ''' <summary>
        ''' 
        ''' </summary>
        Private Repetition As Integer = 1

        ''' <summary>
        ''' Indicates if the linker has been initialized
        ''' </summary>
        ''' <remarks></remarks>
        Private Initialized As Boolean = False

        ''' <summary>
        ''' Indicates if the velocity has to be checked for convergence
        ''' </summary>
        Public ConvergeOnVelocity As Boolean = True

        ''' <summary>
        ''' Sets initial conditions for each ODE
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub Initialize(ByVal Dt As Double, Repetition As Integer)

            ' System starts always from cero position, velocity and acceleration, and the first item represents this state:

            ModalResponse.Add(New ModalCoordinates(StructuralCore.Modes.Count))

            NewmarkIntegrators = New List(Of NewmarkIntegrator)

            For Each Mode In StructuralCore.Modes

                Dim Integrator = New NewmarkIntegrator()
                Integrator.Load(0.25#, 0.5#, Mode.C / Mode.Cc, Mode.W, Dt)
                NewmarkIntegrators.Add(Integrator)

            Next

            Me.T = 0
            Me.Dt = Dt
            Me.Repetition = Repetition

            Initialized = True

        End Sub

        Public Sub UpdateIntegrators()

            Dim M As Integer = 0

            For Each Mode In StructuralCore.Modes

                NewmarkIntegrators(M).Load(0.25#, 0.5#, Mode.C / Mode.Cc, Mode.W, Dt)

                M += 1

            Next

        End Sub

        ''' <summary>
        ''' Advances one time step in the solution of the structural motion with the explicit time integration scheme.
        ''' </summary>
        ''' <param name="Velocity">Reference velocity used to calculate aerodinamic loads</param>
        ''' <remarks>This method integrates the uncoupled ecuations of motion</remarks>
        Public Function ExplicitIntegration(ByVal Velocity As Vector3, ByVal Density As Double) As Boolean

            If Not Initialized Then Throw New Exception("Attempting to integrate with non initialized link")

            T += 1 ' From 1 to ... (t = 0 are the initial conditions, known in advance)

            ' Clear loads and tranfer the new ones to the structure:

            For Each Link In MechanicLinks
                Link.ClearLoads()
                Link.TransferLoads(Velocity, Density)
            Next

            ' Clear last displacements and velocities before setting new ones:

            For Each Node In StructuralCore.Nodes
                Node.Displacement.Clear()
                Node.Velocity.Clear()
            Next

            ' Integrate each uncoupled equation of motion to find new displacements:

            ModalResponse.Add(New ModalCoordinates(StructuralCore.Modes.Count))

            ' Solve equation of motion for each mode and add modal contribution to final response:

            For Each Mode In StructuralCore.Modes

                Dim m As Integer = Mode.Index

                ' 1) Solve each uncoupled ODE by the central difference method:

                Dim L As Double = 0.0#

                For Each Node In StructuralCore.Nodes
                    L += Mode.Shape(Node.Index).VirtualWork(Node.Load)
                Next

                Dim Itegrator As NewmarkIntegrator = NewmarkIntegrators(m)

                Dim P0 As Double = ModalResponse(T - 1)(m).P
                Dim V0 As Double = ModalResponse(T - 1)(m).V
                Dim A0 As Double = ModalResponse(T - 1)(m).A

                ModalResponse(T)(m).A = Itegrator.A(0, 0) * A0 + Itegrator.A(0, 1) * V0 + Itegrator.A(0, 2) * P0 + Itegrator.L(0) * L
                ModalResponse(T)(m).V = Itegrator.A(1, 0) * A0 + Itegrator.A(1, 1) * V0 + Itegrator.A(1, 2) * P0 + Itegrator.L(1) * L
                ModalResponse(T)(m).P = Itegrator.A(2, 0) * A0 + Itegrator.A(2, 1) * V0 + Itegrator.A(2, 2) * P0 + Itegrator.L(2) * L

                ' 2) Calculate nodal displacements by modal superposition:

                Dim n As Integer = 0

                For Each Node In StructuralCore.Nodes

                    n = Node.Index

                    ' Calculate new displacements and velocities:

                    For j = 0 To 5

                        Node.Displacement.Values(j) += ModalResponse(T)(m).P * Mode.Shape(n).Values(j)
                        Node.Velocity.Values(j) += ModalResponse(T)(m).V * Mode.Shape(n).Values(j)

                    Next

                Next

            Next

            ' Transfer motion (position and velocity) to lattices nodal points:

            For Each Link As KinematicLink In KinematicLinks
                Link.TransferMotion()
            Next

            ' Recalculate geometric entities at each lattice control point (CPs, normal, surface velocity)

            For Each Link As MechanicLink In MechanicLinks
                Link.ReloadGeometricEntities()
            Next

            Return True

        End Function

        ''' <summary>
        ''' Completes one itaration in the aeroelastic coupling with the Newmark implicity time integration scheme.
        ''' </summary>
        ''' <param name="Velocity">Reference velocity used to calculate aerodinamic loads</param>
        ''' <param name="Level">Keeps track on the worst level of convergence</param>
        ''' <param name="K">Implicit step counter</param>
        ''' <param name="Delta">Convergence threshold</param>
        ''' <remarks>This method integrates the uncoupled ecuations of motion</remarks>
        ''' <returns>True when the relative increment of the new prediction in all modal displacements is less than e.</returns>
        Public Function ImplicitIntegration(ByVal Velocity As Vector3, ByVal Density As Double, ByRef Level As Double, AdvanceStep As Boolean, Delta As Double) As Boolean

            If Not Initialized Then Throw New Exception("Attempting to integrate with non initialized link")

            ' Clear loads and tranfer the new ones to the structure:

            For Each Link In MechanicLinks
                Link.ClearLoads()
                Link.TransferLoads(Velocity, Density)
            Next

            ' Clear last displacements and velocities before setting new ones:

            For Each Node In StructuralCore.Nodes
                Node.Displacement.Clear()
                Node.Velocity.Clear()
            Next

            ' Add new response element and advance one time step when requested

            If AdvanceStep Then

                T += 1 ' From 1 to ... (t = 0 are the initial conditions, known in advance)

                ModalResponse.Add(New ModalCoordinates(StructuralCore.Modes.Count))

            End If

            Dim Converged As Boolean = True

            ' Solve equation of motion for each mode and add modal contribution to final response:

            For Each Mode In StructuralCore.Modes

                Dim M As Integer = Mode.Index

                ' 1) Solve each uncoupled ODE by the Newmark method:

                Dim Work As Double = 0.0#

                For Each Node In StructuralCore.Nodes

                    Work += Mode.Shape(Node.Index).VirtualWork(Node.Load)

                Next

                Dim Int As NewmarkIntegrator = NewmarkIntegrators(M)

                Dim P0 As Double = ModalResponse(T - 1)(M).P
                Dim V0 As Double = ModalResponse(T - 1)(M).V
                Dim A0 As Double = ModalResponse(T - 1)(M).A

                ' Cache last predicted position

                Dim PreviousP As Double
                Dim PreviousV As Double

                If AdvanceStep Then
                    PreviousP = P0
                    PreviousV = V0
                Else
                    PreviousP = ModalResponse(T)(M).P
                    PreviousV = ModalResponse(T)(M).V
                End If

                ' Complete the time step using the sub partition (constant load durig the small interval)

                ModalResponse(T)(M).A = 0.0#
                ModalResponse(T)(M).V = 0.0#
                ModalResponse(T)(M).P = 0.0#
                ModalResponse(T)(M).W = Work

                For R = 1 To Repetition

                    ModalResponse(T)(M).A = Int.A(0, 0) * A0 + Int.A(0, 1) * V0 + Int.A(0, 2) * P0 + Int.L(0) * Work
                    ModalResponse(T)(M).V = Int.A(1, 0) * A0 + Int.A(1, 1) * V0 + Int.A(1, 2) * P0 + Int.L(1) * Work
                    ModalResponse(T)(M).P = Int.A(2, 0) * A0 + Int.A(2, 1) * V0 + Int.A(2, 2) * P0 + Int.L(2) * Work

                    P0 = ModalResponse(T)(M).A
                    V0 = ModalResponse(T)(M).V
                    A0 = ModalResponse(T)(M).P

                Next

                ' Check converence using previous prediction

                If PreviousP <> 0 Then

                    Dim ConvergenceP As Double = (ModalResponse(T)(M).P - PreviousP) / PreviousP
                    Converged = Converged And ConvergenceP < Delta
                    Level = Math.Max(Level, ConvergenceP)

                End If

                If ConvergeOnVelocity Then

                    If PreviousV <> 0 Then
                        Dim ConvergenceV As Double = (ModalResponse(T)(M).V - PreviousV) / PreviousV
                        Converged = Converged And ConvergenceV < Delta
                        Level = Math.Max(Level, ConvergenceV)
                    End If

                End If

                ' 2) Calculate nodal displacements by modal superposition:

                Dim n As Integer = 0

                For Each Node In StructuralCore.Nodes

                    n = Node.Index

                    ' Calculate new displacements and velocities:

                    For j = 0 To 5

                        Node.Displacement.Values(j) += ModalResponse(T)(M).P * Mode.Shape(n).Values(j)
                        Node.Velocity.Values(j) += ModalResponse(T)(M).V * Mode.Shape(n).Values(j)

                    Next

                Next

            Next

            ' Transfer motion (position and velocity) to lattices nodal points:

            For Each Link As KinematicLink In KinematicLinks

                Link.TransferMotion()

            Next

            ' Recalculate geometric entities at each lattice control point (CPs, normal, surface velocity)

            For Each Link As MechanicLink In MechanicLinks
                Link.ReloadGeometricEntities()
            Next

            Return Converged

        End Function

        ''' <summary>
        ''' Writes linker to a binary file.
        ''' </summary>
        ''' <param name="Path"></param>
        ''' <remarks></remarks>
        Public Sub WriteBinary(ByVal Path As String)

            Dim w As BinaryWriter = New BinaryWriter(File.Open(Path, FileMode.Create))

            ' Write structure:

            w.Write(StructuralCore.Nodes.Count)

            For Each n In StructuralCore.Nodes

                w.Write(n.Position.X)
                w.Write(n.Position.Y)
                w.Write(n.Contrains.FixedDx)
                w.Write(n.Contrains.FixedDy)
                w.Write(n.Contrains.FixedDz)
                w.Write(n.Contrains.FixedRx)
                w.Write(n.Contrains.FixedRy)
                w.Write(n.Contrains.FixedRz)
                w.Write(n.Displacement.Dx)
                w.Write(n.Displacement.Dy)
                w.Write(n.Displacement.Dz)
                w.Write(n.Displacement.Rx)
                w.Write(n.Displacement.Ry)
                w.Write(n.Displacement.Rz)
                w.Write(n.Load.Px)
                w.Write(n.Load.Py)
                w.Write(n.Load.Pz)
                w.Write(n.Load.Tx)
                w.Write(n.Load.Ty)
                w.Write(n.Load.Tz)
                w.Write(n.Velocity.Dx)
                w.Write(n.Velocity.Dy)
                w.Write(n.Velocity.Dz)
                w.Write(n.Velocity.Rx)
                w.Write(n.Velocity.Ry)
                w.Write(n.Velocity.Rz)

            Next

            ' Write elements:

            w.Write(StructuralCore.Elements.Count)

            For Each e In StructuralCore.Elements

                w.Write(e.NodeA.Index)
                w.Write(e.NodeB.Index)

                w.Write(e.Basis.U.X)
                w.Write(e.Basis.U.Y)
                w.Write(e.Basis.U.Z)

                w.Write(e.Basis.V.X)
                w.Write(e.Basis.V.Y)
                w.Write(e.Basis.V.Z)

                w.Write(e.Basis.W.X)
                w.Write(e.Basis.W.Y)
                w.Write(e.Basis.W.Z)

                w.Write(e.Section.AE)
                w.Write(e.Section.EIy)
                w.Write(e.Section.EIz)
                w.Write(e.Section.GJ)
                w.Write(e.Section.M)
                w.Write(e.Section.Ip)

            Next

            ' Write modes:

            w.Write(StructuralCore.Modes.Count)

            For i = 0 To StructuralCore.Modes.Count - 1 ' < For each mode
                w.Write(StructuralCore.Modes(i).K)
                w.Write(StructuralCore.Modes(i).M)
                w.Write(StructuralCore.Modes(i).W)
                w.Write(StructuralCore.Modes(i).C)
                w.Write(StructuralCore.Modes(i).Cc)
                w.Write(StructuralCore.Modes(i).Shape.Count)
                For j = 0 To StructuralCore.Modes(i).Shape.Count - 1
                    w.Write(StructuralCore.Modes(i).Shape(j).Dx)
                    w.Write(StructuralCore.Modes(i).Shape(j).Dy)
                    w.Write(StructuralCore.Modes(i).Shape(j).Dz)
                    w.Write(StructuralCore.Modes(i).Shape(j).Rx)
                    w.Write(StructuralCore.Modes(i).Shape(j).Ry)
                    w.Write(StructuralCore.Modes(i).Shape(j).Rz)
                Next
            Next

            ' Write kinematic links:

            w.Write(KinematicLinks.Count)

            For Each kl In KinematicLinks

                w.Write(kl.LinkedNode.Index)
                w.Write(kl.LinkedStripe.Count)

                For j = 0 To kl.LinkedStripe.Count - 1
                    w.Write(kl.LinkedStripe(j).IndexG)
                Next

            Next

            ' Write mechanic links:

            w.Write(MechanicLinks.Count)

            For Each ml In MechanicLinks

                w.Write(ml.LinkedElement.Index)
                w.Write(ml.LinkedStripe.Count)

                For j = 0 To ml.LinkedStripe.Count - 1
                    w.Write(ml.LinkedStripe(j).IndexG)
                Next

            Next

            ' Response:

            w.Write(ModalResponse.Count)

            For Each Response In ModalResponse ' < For each time step

                For j = 0 To StructuralCore.Modes.Count - 1 ' < For each mode

                    w.Write(Response.Item(j).P)
                    w.Write(Response.Item(j).V)
                    w.Write(Response.Item(j).A)

                Next

            Next

            w.Close()

        End Sub

        ''' <summary>
        ''' Reads the linker from a binary file.
        ''' </summary>
        ''' <param name="Path"></param>
        ''' <remarks></remarks>
        Public Sub ReadBinary(ByVal Path As String, ByRef Nodes As List(Of Node), ByRef Rings As List(Of VortexRing))

            Dim r As BinaryReader = New BinaryReader(File.Open(Path, FileMode.Open))

            ' Read nodes:

            Dim nn As Integer = r.ReadInt32

            For i = 0 To nn - 1

                Dim node As New StructuralNode(i)

                node.Position.X = r.ReadDouble
                node.Position.Y = r.ReadDouble
                node.Contrains.FixedDx = r.ReadBoolean
                node.Contrains.FixedDy = r.ReadBoolean
                node.Contrains.FixedDz = r.ReadBoolean
                node.Contrains.FixedRx = r.ReadBoolean
                node.Contrains.FixedRy = r.ReadBoolean
                node.Contrains.FixedRz = r.ReadBoolean
                node.Displacement.Dx = r.ReadDouble
                node.Displacement.Dy = r.ReadDouble
                node.Displacement.Dz = r.ReadDouble
                node.Displacement.Rx = r.ReadDouble
                node.Displacement.Ry = r.ReadDouble
                node.Displacement.Rz = r.ReadDouble
                node.Load.Px = r.ReadDouble
                node.Load.Py = r.ReadDouble
                node.Load.Pz = r.ReadDouble
                node.Load.Tx = r.ReadDouble
                node.Load.Ty = r.ReadDouble
                node.Load.Tz = r.ReadDouble
                node.Velocity.Dx = r.ReadDouble
                node.Velocity.Dy = r.ReadDouble
                node.Velocity.Dz = r.ReadDouble
                node.Velocity.Rx = r.ReadDouble
                node.Velocity.Ry = r.ReadDouble
                node.Velocity.Rz = r.ReadDouble

                StructuralCore.Nodes.Add(node)

            Next

            ' Read elements:

            Dim ne As Integer = r.ReadInt32

            For i = 0 To ne - 1

                Dim element As New ConstantBeamElement(i)

                element.NodeA = StructuralCore.Nodes(r.ReadUInt32)
                element.NodeB = StructuralCore.Nodes(r.ReadUInt32)

                element.Basis.U.X = r.ReadDouble
                element.Basis.U.Y = r.ReadDouble
                element.Basis.U.Z = r.ReadDouble

                element.Basis.V.X = r.ReadDouble
                element.Basis.V.Y = r.ReadDouble
                element.Basis.V.Z = r.ReadDouble

                element.Basis.W.X = r.ReadDouble
                element.Basis.W.Y = r.ReadDouble
                element.Basis.W.Z = r.ReadDouble

                element.Section.AE = r.ReadDouble
                element.Section.EIy = r.ReadDouble
                element.Section.EIz = r.ReadDouble
                element.Section.GJ = r.ReadDouble
                element.Section.M = r.ReadDouble
                element.Section.Ip = r.ReadDouble

                StructuralCore.Elements.Add(element)

            Next

            ' Read modes:

            StructuralCore.Modes.Clear()

            Dim nm As Integer = r.ReadInt32

            For i = 0 To nm - 1

                Dim Mode As New Mode(i)

                Mode.K = r.ReadDouble
                Mode.M = r.ReadDouble
                Mode.W = r.ReadDouble
                Mode.C = r.ReadDouble
                Mode.Cc = r.ReadDouble

                Dim ss As Integer = r.ReadInt32

                For j = 0 To ss - 1
                    Dim displacement As New NodalDisplacement()
                    displacement.Dx = r.ReadDouble
                    displacement.Dy = r.ReadDouble
                    displacement.Dz = r.ReadDouble
                    displacement.Rx = r.ReadDouble
                    displacement.Ry = r.ReadDouble
                    displacement.Rz = r.ReadDouble
                    Mode.Shape.Add(displacement)
                Next

                StructuralCore.Modes.Add(Mode)

            Next

            ' Read kinematic links:

            Dim nkl As Integer = r.ReadInt32

            For i = 0 To nkl - 1

                Dim kl As New KinematicLink(StructuralCore.Nodes(r.ReadInt32))

                Dim sl As Integer = r.ReadInt32

                For j = 0 To sl - 1
                    kl.LinkedStripe.Add(Nodes(r.ReadInt32))
                Next

                KinematicLinks.Add(kl)

            Next

            ' Read mechanic links:

            Dim nml As Integer = r.ReadInt32

            For i = 0 To nml - 1

                Dim ml As New MechanicLink(StructuralCore.Elements(r.ReadInt32))

                Dim sl As Integer = r.ReadInt32

                For j = 0 To sl - 1
                    ml.LinkedStripe.Add(Rings(r.ReadInt32))
                Next

                MechanicLinks.Add(ml)

            Next

            ' Read response:

            Dim nmr As Integer = r.ReadInt32

            For i = 0 To nmr - 1 ' < For each time step

                Dim modalCoords As New ModalCoordinates(nm)

                For j = 0 To nm - 1 ' < For each mode

                    Dim modalCoord As New ModalCoordinate()

                    modalCoord.P = r.ReadDouble
                    modalCoord.V = r.ReadDouble
                    modalCoord.A = r.ReadDouble

                    modalCoords.Item(j) = modalCoord

                Next

                ModalResponse.Add(modalCoords)

            Next

            r.Close()

        End Sub

    End Class

End Namespace

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

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Provides a kinematic link between structural and aerodynamic models with 6 DOFs
    '//////////////////////////////////////////////////////////////////////////////////////////
    ' <remarks>This class is able to convert the motion of an structural node to the nodes of a node-stripe</remarks>
    Public Class KinematicLink

        Private _LinkedStripe As List(Of Node)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Collection of aerodynamic nodal points (deformed structure)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property LinkedStripe As List(Of Node)
            Get
                Return _LinkedStripe
            End Get
        End Property

        Private _LinkedNode As StructuralNode
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Structural node which will transfer motion to the linked node-stripe
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property LinkedNode As StructuralNode
            Get
                Return _LinkedNode
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Creates a kinematic link
        ' LinkedNode is the structural node to be linked
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New(ByVal LinkedNode As StructuralNode, ByRef LinkedStripe As List(Of Node))

            _LinkedStripe = LinkedStripe
            _LinkedNode = LinkedNode

        End Sub
        '------------------------------------------------------------------------------------------



        '//////////////////////////////////////////////////////////////////////////////////////////
        '
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New(ByVal LinkedNode As StructuralNode)

            _LinkedStripe = New List(Of Node)
            _LinkedNode = LinkedNode

        End Sub
        '------------------------------------------------------------------------------------------



        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Links a given aerodynamic node to the structural node associated to this link.
        ' Node is th aerodinamic node to be linked
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub Link(ByRef Node As Node)

            ' Add reference to the node:

            _LinkedStripe.Add(Node)

            ' Save the current position as reference position:

            Node.OriginalPosition = New Vector3(Node.Position)

            ' Initialize nodal displacement

            Node.Displacement = New Vector3

        End Sub
        '------------------------------------------------------------------------------------------



        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Transfer the motion from the structural model to the aerodynamic model
        '//////////////////////////////////////////////////////////////////////////////////////////
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
        '------------------------------------------------------------------------------------------

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Provides a mechanic link throug which aerodynamic loads are transfered to the
    ' structural model
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class MechanicLink

        ' This link calculates the local Cm, Cl and Cd and transfer them to both element nodes

        Private _LinkedStripe As List(Of VortexRing)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Collection of vortex rings which will transfer loads to the structure
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property LinkedStripe As List(Of VortexRing)
            Get
                Return _LinkedStripe
            End Get
        End Property

        Private _LinkedElement As BeamElement
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Structural element which will recieve the loads from the panel-stripe
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property LinkedElement As BeamElement
            Get
                Return _LinkedElement
            End Get
        End Property

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Polar As IPolarCurve

        '==========================================================================================
        '
        '==========================================================================================
        Public Sub New(ByVal LinkedElement As BeamElement, ByRef LinkedStripe As List(Of VortexRing))

            _LinkedElement = LinkedElement
            _LinkedStripe = LinkedStripe

        End Sub
        
        '==========================================================================================
        '
        '==========================================================================================
        Public Sub New(ByVal LinkedElement As BeamElement)

            _LinkedElement = LinkedElement
            _LinkedStripe = New List(Of VortexRing)

        End Sub
        '------------------------------------------------------------------------------------------

        '==========================================================================================
        ' Links a vortex ring to the structural element associated to this link.
        '==========================================================================================
        Public Sub Link(ByRef Ring As VortexRing)

            _LinkedStripe.Add(Ring)

        End Sub
        '------------------------------------------------------------------------------------------

        '==========================================================================================
        ' Transfer loads from the aerodynamic model to the structural model.
        ' Velocity is the reference velocity
        ' TODO: this metod should take into account the missing leading edge pressure decay
        ' by substracting the projection of the force in the stream velocity direction.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------

        '==========================================================================================
        ' Sets the loads to cero
        '==========================================================================================
        Public Sub ClearLoads()
            _LinkedElement.NodeA.Load.Clear()
            _LinkedElement.NodeB.Load.Clear()
        End Sub
        '------------------------------------------------------------------------------------------

        '==========================================================================================
        ' Resets normal vectors and control points on linked rings
        '==========================================================================================
        Public Sub ReloadGeometricEntities()

            For Each Ring In _LinkedStripe

                Ring.CalculateGeometricEntities()

            Next

        End Sub
        '------------------------------------------------------------------------------------------

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Simulates an aeroelastic interaction by linkin the structural model to the aerodynamic model
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class StructuralLink

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' System structure
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public StructuralCore As StructuralCore

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Collection of kinematic links
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public KinematicLinks As List(Of KinematicLink)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Collection of mechanic links
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public MechanicLinks As List(Of MechanicLink)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Collection of modal coordinates for each time-step
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ModalResponse As List(Of ModalCoordinates)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' List of Newmark inegrators for each mode.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private NewmarkIntegrators As List(Of NewmarkIntegrator)

        '//////////////////////////////////////////////////////////////////////////////////////////
        '
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New()

            StructuralCore = New StructuralCore
            KinematicLinks = New List(Of KinematicLink)
            MechanicLinks = New List(Of MechanicLink)
            ModalResponse = New List(Of ModalCoordinates)

        End Sub
        '------------------------------------------------------------------------------------------

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Current time step
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private T As Integer = 0

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Time interval.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private Dt As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' 
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private Repetition As Integer = 1

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the linker has been initialized
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private Initialized As Boolean = False

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the velocity has to be checked for convergence
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ConvergeOnVelocity As Boolean = True

        '==========================================================================================
        ' Sets initial conditions for each ODE
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        '
        '==========================================================================================
        Public Sub UpdateIntegrators()

            Dim M As Integer = 0

            For Each Mode In StructuralCore.Modes

                NewmarkIntegrators(M).Load(0.25#, 0.5#, Mode.C / Mode.Cc, Mode.W, Dt)

                M += 1

            Next

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Advances one time step in the solution of the structural motion with the explicit time 
        ' integration scheme.
        ' Velocity: reference velocity used to calculate aerodinamic loads.
        ' This method integrates the uncoupled ecuations of motion
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' This method integrates the uncoupled ecuations of motion by completing one itaration
        ' in the aeroelastic coupling with the Newmark implicity time integration scheme.
        ' Velocity: reference velocity used to calculate aerodinamic loads.
        ' Level:    keeps track on the worst level of convergence
        ' K:        Implicit step counter
        ' Delta:    Convergence threshold
        ' The function returns true when the relative increment of the new prediction in all modal 
        ' displacements is less than e.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Writes linker to a binary file.
        '==========================================================================================
        Public Sub WriteBinary(ByVal Path As String)

            Dim Stream As BinaryWriter = New BinaryWriter(File.Open(Path, FileMode.Create))

            ' Write structure:

            Stream.Write(StructuralCore.Nodes.Count)

            For Each n In StructuralCore.Nodes

                Stream.Write(n.Position.X)
                Stream.Write(n.Position.Y)
                Stream.Write(n.Contrains.FixedDx)
                Stream.Write(n.Contrains.FixedDy)
                Stream.Write(n.Contrains.FixedDz)
                Stream.Write(n.Contrains.FixedRx)
                Stream.Write(n.Contrains.FixedRy)
                Stream.Write(n.Contrains.FixedRz)
                Stream.Write(n.Displacement.Dx)
                Stream.Write(n.Displacement.Dy)
                Stream.Write(n.Displacement.Dz)
                Stream.Write(n.Displacement.Rx)
                Stream.Write(n.Displacement.Ry)
                Stream.Write(n.Displacement.Rz)
                Stream.Write(n.Load.Px)
                Stream.Write(n.Load.Py)
                Stream.Write(n.Load.Pz)
                Stream.Write(n.Load.Tx)
                Stream.Write(n.Load.Ty)
                Stream.Write(n.Load.Tz)
                Stream.Write(n.Velocity.Dx)
                Stream.Write(n.Velocity.Dy)
                Stream.Write(n.Velocity.Dz)
                Stream.Write(n.Velocity.Rx)
                Stream.Write(n.Velocity.Ry)
                Stream.Write(n.Velocity.Rz)

            Next

            ' Write elements:

            Stream.Write(StructuralCore.Elements.Count)

            For Each e In StructuralCore.Elements

                Stream.Write(e.NodeA.Index)
                Stream.Write(e.NodeB.Index)

                Stream.Write(e.Basis.U.X)
                Stream.Write(e.Basis.U.Y)
                Stream.Write(e.Basis.U.Z)

                Stream.Write(e.Basis.V.X)
                Stream.Write(e.Basis.V.Y)
                Stream.Write(e.Basis.V.Z)

                Stream.Write(e.Basis.W.X)
                Stream.Write(e.Basis.W.Y)
                Stream.Write(e.Basis.W.Z)

                Stream.Write(e.Section.AE)
                Stream.Write(e.Section.EIy)
                Stream.Write(e.Section.EIz)
                Stream.Write(e.Section.GJ)
                Stream.Write(e.Section.M)
                Stream.Write(e.Section.Ip)

            Next

            ' Write modes:

            Stream.Write(StructuralCore.Modes.Count)

            For i = 0 To StructuralCore.Modes.Count - 1 ' < For each mode
                Stream.Write(StructuralCore.Modes(i).K)
                Stream.Write(StructuralCore.Modes(i).M)
                Stream.Write(StructuralCore.Modes(i).W)
                Stream.Write(StructuralCore.Modes(i).C)
                Stream.Write(StructuralCore.Modes(i).Cc)
                Stream.Write(StructuralCore.Modes(i).Shape.Count)
                For j = 0 To StructuralCore.Modes(i).Shape.Count - 1
                    Stream.Write(StructuralCore.Modes(i).Shape(j).Dx)
                    Stream.Write(StructuralCore.Modes(i).Shape(j).Dy)
                    Stream.Write(StructuralCore.Modes(i).Shape(j).Dz)
                    Stream.Write(StructuralCore.Modes(i).Shape(j).Rx)
                    Stream.Write(StructuralCore.Modes(i).Shape(j).Ry)
                    Stream.Write(StructuralCore.Modes(i).Shape(j).Rz)
                Next
            Next

            ' Write kinematic links:

            Stream.Write(KinematicLinks.Count)

            For Each kl In KinematicLinks

                Stream.Write(kl.LinkedNode.Index)
                Stream.Write(kl.LinkedStripe.Count)

                For j = 0 To kl.LinkedStripe.Count - 1
                    Stream.Write(kl.LinkedStripe(j).IndexG)
                Next

            Next

            ' Write mechanic links:

            Stream.Write(MechanicLinks.Count)

            For Each ml In MechanicLinks

                Stream.Write(ml.LinkedElement.Index)
                Stream.Write(ml.LinkedStripe.Count)

                For j = 0 To ml.LinkedStripe.Count - 1
                    Stream.Write(ml.LinkedStripe(j).IndexG)
                Next

            Next

            ' Response:

            Stream.Write(ModalResponse.Count)

            For Each Response In ModalResponse ' < For each time step

                For j = 0 To StructuralCore.Modes.Count - 1 ' < For each mode

                    Stream.Write(Response.Item(j).P)
                    Stream.Write(Response.Item(j).V)
                    Stream.Write(Response.Item(j).A)

                Next

            Next

            Stream.Close()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the linker from a binary file.
        '==========================================================================================
        Public Sub ReadBinary(ByVal Path As String, ByRef Nodes As List(Of Node), ByRef Rings As List(Of VortexRing))

            Dim Stream As BinaryReader = New BinaryReader(File.Open(Path, FileMode.Open))

            ' Read nodes:

            Dim nn As Integer = Stream.ReadInt32

            For i = 0 To nn - 1

                Dim NewNode As New StructuralNode(i)

                NewNode.Position.X = Stream.ReadDouble
                NewNode.Position.Y = Stream.ReadDouble
                NewNode.Contrains.FixedDx = Stream.ReadBoolean
                NewNode.Contrains.FixedDy = Stream.ReadBoolean
                NewNode.Contrains.FixedDz = Stream.ReadBoolean
                NewNode.Contrains.FixedRx = Stream.ReadBoolean
                NewNode.Contrains.FixedRy = Stream.ReadBoolean
                NewNode.Contrains.FixedRz = Stream.ReadBoolean
                NewNode.Displacement.Dx = Stream.ReadDouble
                NewNode.Displacement.Dy = Stream.ReadDouble
                NewNode.Displacement.Dz = Stream.ReadDouble
                NewNode.Displacement.Rx = Stream.ReadDouble
                NewNode.Displacement.Ry = Stream.ReadDouble
                NewNode.Displacement.Rz = Stream.ReadDouble
                NewNode.Load.Px = Stream.ReadDouble
                NewNode.Load.Py = Stream.ReadDouble
                NewNode.Load.Pz = Stream.ReadDouble
                NewNode.Load.Tx = Stream.ReadDouble
                NewNode.Load.Ty = Stream.ReadDouble
                NewNode.Load.Tz = Stream.ReadDouble
                NewNode.Velocity.Dx = Stream.ReadDouble
                NewNode.Velocity.Dy = Stream.ReadDouble
                NewNode.Velocity.Dz = Stream.ReadDouble
                NewNode.Velocity.Rx = Stream.ReadDouble
                NewNode.Velocity.Ry = Stream.ReadDouble
                NewNode.Velocity.Rz = Stream.ReadDouble

                StructuralCore.Nodes.Add(NewNode)

            Next

            ' Read elements:

            Dim ne As Integer = Stream.ReadInt32

            For i = 0 To ne - 1

                Dim NewElement As New ConstantBeamElement(i)

                NewElement.NodeA = StructuralCore.Nodes(Stream.ReadUInt32)
                NewElement.NodeB = StructuralCore.Nodes(Stream.ReadUInt32)

                NewElement.Basis.U.X = Stream.ReadDouble
                NewElement.Basis.U.Y = Stream.ReadDouble
                NewElement.Basis.U.Z = Stream.ReadDouble

                NewElement.Basis.V.X = Stream.ReadDouble
                NewElement.Basis.V.Y = Stream.ReadDouble
                NewElement.Basis.V.Z = Stream.ReadDouble

                NewElement.Basis.W.X = Stream.ReadDouble
                NewElement.Basis.W.Y = Stream.ReadDouble
                NewElement.Basis.W.Z = Stream.ReadDouble

                NewElement.Section.AE = Stream.ReadDouble
                NewElement.Section.EIy = Stream.ReadDouble
                NewElement.Section.EIz = Stream.ReadDouble
                NewElement.Section.GJ = Stream.ReadDouble
                NewElement.Section.M = Stream.ReadDouble
                NewElement.Section.Ip = Stream.ReadDouble

                StructuralCore.Elements.Add(NewElement)

            Next

            ' Read modes:

            StructuralCore.Modes.Clear()

            Dim nm As Integer = Stream.ReadInt32

            For i = 0 To nm - 1

                Dim NewMode As New Mode(i)

                NewMode.K = Stream.ReadDouble
                NewMode.M = Stream.ReadDouble
                NewMode.W = Stream.ReadDouble
                NewMode.C = Stream.ReadDouble
                NewMode.Cc = Stream.ReadDouble

                Dim ss As Integer = Stream.ReadInt32

                For j = 0 To ss - 1
                    Dim Displacements As New NodalDisplacement()
                    Displacements.Dx = Stream.ReadDouble
                    Displacements.Dy = Stream.ReadDouble
                    Displacements.Dz = Stream.ReadDouble
                    Displacements.Rx = Stream.ReadDouble
                    Displacements.Ry = Stream.ReadDouble
                    Displacements.Rz = Stream.ReadDouble
                    NewMode.Shape.Add(Displacements)
                Next

                StructuralCore.Modes.Add(NewMode)

            Next

            ' Read kinematic links:

            Dim nkl As Integer = Stream.ReadInt32

            For i = 0 To nkl - 1

                Dim kl As New KinematicLink(StructuralCore.Nodes(Stream.ReadInt32))

                Dim sl As Integer = Stream.ReadInt32

                For j = 0 To sl - 1
                    kl.LinkedStripe.Add(Nodes(Stream.ReadInt32))
                Next

                KinematicLinks.Add(kl)

            Next

            ' Read mechanic links:

            Dim nml As Integer = Stream.ReadInt32

            For i = 0 To nml - 1

                Dim ml As New MechanicLink(StructuralCore.Elements(Stream.ReadInt32))

                Dim sl As Integer = Stream.ReadInt32

                For j = 0 To sl - 1
                    ml.LinkedStripe.Add(Rings(Stream.ReadInt32))
                Next

                MechanicLinks.Add(ml)

            Next

            ' Read response:

            Dim nmr As Integer = Stream.ReadInt32

            For i = 0 To nmr - 1 ' < For each time step

                Dim NewModalCoordinates As New ModalCoordinates(nm)

                For j = 0 To nm - 1 ' < For each mode

                    Dim modalCoord As New ModalCoordinate()

                    modalCoord.P = Stream.ReadDouble
                    modalCoord.V = Stream.ReadDouble
                    modalCoord.A = Stream.ReadDouble

                    NewModalCoordinates.Item(j) = modalCoord

                Next

                ModalResponse.Add(NewModalCoordinates)

            Next

            Stream.Close()

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------

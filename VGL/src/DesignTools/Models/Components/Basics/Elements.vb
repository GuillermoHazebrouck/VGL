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

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports VGL.AeroTools.Settings
Imports VGL.DesignTools.Tools.Colormaping

'#############################################################################
' Unit: Elements
'
' This unit provides a generic surface element
'#############################################################################
Namespace DesignTools.Models.Components.Basics

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a panel (both, triangular or quadrialteral) on the model.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Panel

        '==========================================================================================
        '  Creates a new panel
        '==========================================================================================
        Public Sub New()
            ControlPoint = New Vector3
            NormalVector = New Vector3
            LocalVelocity = New Vector3
            Area = 0.0#
            Circulation = 0.0#
            Cp = 0.0#
            GlobalIndex = 0
            IsReversed = False
            IsPrimitive = False
        End Sub
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The index of the first node
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property N1 As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The index of the second node
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property N2 As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The index of the third node
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property N3 As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The index of the fourth node (only for quadrilateral panels)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property N4 As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The global index in the model
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property GlobalIndex As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The control point
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property ControlPoint As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The normal vector
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property NormalVector As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The area of the panel
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Area As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if this panel has a reversed singularity behavior
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property IsReversed As Boolean

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The local velocity vector at the control point
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property LocalVelocity As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The local circulation
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Circulation As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The local sink/source strenght
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SourceStrength As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The panel pressure coefficient
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Cp As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The color associated to te pressure coefficient
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CpColor As ColorSharpGL

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if this panel is a primitive convection area
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property IsPrimitive As Boolean

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if this panel represents a slender (zero thickness) surface
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property IsSlender As Boolean = True

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if this panel is active (for selection)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Active As Boolean = False

        Private _IsTriangular As Boolean
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if this panel is represented by only the first three nodes.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property IsTriangular As Boolean
            Get
                Return _IsTriangular
            End Get
        End Property

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The velocity induced on this panel
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private _InducedVelocity As New Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a quadrilateral element
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New(ByVal N1 As Integer, ByVal N2 As Integer, ByVal N3 As Integer, ByVal N4 As Integer)

            Me.N1 = N1
            Me.N2 = N2
            Me.N3 = N3
            Me.N4 = N4
            _IsTriangular = False

        End Sub
        '------------------------------------------------------------------------------------------

        '==========================================================================================
        ' Generates a triangular element
        '==========================================================================================
        Public Sub New(ByVal N1 As Integer, ByVal N2 As Integer, ByVal N3 As Integer)

            Me.N1 = N1
            Me.N2 = N2
            Me.N3 = N3
            Me.N4 = N1
            _IsTriangular = True

        End Sub
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The velocity induced at the control point of this panel
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property InducedVelocity As Vector3
            Get
                Return _InducedVelocity
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        '==========================================================================================
        ' Deep copy of the given object into this one
        '==========================================================================================
        Public Sub Assign(ByVal Panel As Panel)

            N1 = Panel.N1
            N2 = Panel.N2
            N3 = Panel.N3
            N4 = Panel.N4

            _IsTriangular = Panel.IsTriangular

            GlobalIndex = Panel.GlobalIndex
            ControlPoint.Assign(Panel.ControlPoint)
            NormalVector.Assign(Panel.NormalVector)
            Area = Panel.Area
            IsReversed = Panel.IsReversed

            LocalVelocity.Assign(Panel.LocalVelocity)
            Circulation = Panel.Circulation
            Cp = Panel.Cp

            IsPrimitive = Panel.IsPrimitive
            IsSlender = Panel.IsSlender

            _InducedVelocity.Assign(Panel.InducedVelocity)

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '------------------------------------------------------------------------------------------


    
    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a point in a view model. This object is used for modelling and representation of results.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class NodalPoint

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicqtes if this node is active (selection)
        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Active As Boolean = False

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The original position of this node (used to represent the undeformed state)
        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property ReferencePosition As Vector3

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The position of this node
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Position As Vector3

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The displacement (for modes or aeroelastic results)
        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Displacement As Vector3

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The color of the displacement
        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property DisplacementColor As ColorSharpGL

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The interpolated pressure coefficient
        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Cp As Double

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The color of the pressure coefficient
        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CpColor As ColorSharpGL

        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The interpolated pressure jump
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CpDelta As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The color of the interpolated pressure jump
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CpDeltaColor As ColorSharpGL

        '==========================================================================================
        ' Creates a new node with coordinates (0,0,0)
        '==========================================================================================
        Public Sub New()
            Position = New Vector3
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Creates a new node at the given coordinates
        '==========================================================================================
        Public Sub New(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)
            Position = New Vector3(X, Y, Z)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Creates a new node at the position of point P
        '==========================================================================================
        Public Sub New(ByRef P As Vector3)
            Position = New Vector3(P)
        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '------------------------------------------------------------------------------------------



    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a segment in the lattice
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class LatticeSegment

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The index of the first node
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public N1 As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The index of the second node
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public N2 As Integer

        '==========================================================================================
        ' Creates a new segment referencing to node 0
        '==========================================================================================
        Public Sub New()
            N1 = 0
            N2 = 0
        End Sub
        '------------------------------------------------------------------------------------------

        '==========================================================================================
        ' Creates a new segment linked to nodes N1 and N2 
        '==========================================================================================
        Public Sub New(ByVal N1 As Integer, ByVal N2 As Integer)
            Me.N1 = N1
            Me.N2 = N2
        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '------------------------------------------------------------------------------------------
End Namespace
'------------------------------------------------------------------------------------------
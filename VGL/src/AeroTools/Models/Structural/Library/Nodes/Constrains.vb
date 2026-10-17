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

'#############################################################################
' Unit: Constraints
'
' This unit declares the contrains that can be imposed on a structural node
'#############################################################################
Namespace AeroTools.Models.Structural.Library.Nodes

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Nodal constrains
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Constrains

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates which degrees of freedom in the node are fixed to the inertial reference frame.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Fixed(5) As Boolean

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates which degrees of freedom in the node are attached to a spring
        ' on the inertial reference frame.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public K(5) As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the displacement in the X direction is restrained.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FixedDx As Boolean
            Set(ByVal value As Boolean)
                Fixed(0) = value
            End Set
            Get
                Return Fixed(0)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the displacement in the Y direction is restrained.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FixedDy As Boolean
            Set(ByVal value As Boolean)
                Fixed(1) = value
            End Set
            Get
                Return Fixed(1)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the displacement in the Z direction is restrained.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FixedDz As Boolean
            Set(ByVal value As Boolean)
                Fixed(2) = value
            End Set
            Get
                Return Fixed(2)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the rotation about the X axis is restrained.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FixedRx As Boolean
            Set(ByVal value As Boolean)
                Fixed(3) = value
            End Set
            Get
                Return Fixed(3)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the rotation about the Y axis is restrained.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FixedRy As Boolean
            Set(ByVal value As Boolean)
                Fixed(4) = value
            End Set
            Get
                Return Fixed(4)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the rotation about the Z axis is restrained.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FixedRz As Boolean
            Set(ByVal value As Boolean)
                Fixed(5) = value
            End Set
            Get
                Return Fixed(5)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stiffness in X direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property KDx As Double
            Set(ByVal value As Double)
                K(0) = value
            End Set
            Get
                Return K(0)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stiffness in Y direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property KDy As Double
            Set(ByVal value As Double)
                K(1) = value
            End Set
            Get
                Return K(1)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stiffness in Z direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property KDz As Double
            Set(ByVal value As Double)
                K(2) = value
            End Set
            Get
                Return K(2)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stiffness arround X direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property KRx As Double
            Set(ByVal value As Double)
                K(3) = value
            End Set
            Get
                Return K(3)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stiffness arround Y direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property KRy As Double
            Set(ByVal value As Double)
                K(4) = value
            End Set
            Get
                Return K(4)
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stiffness arround Z direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property KRz As Double
            Set(ByVal value As Double)
                K(5) = value
            End Set
            Get
                Return K(5)
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Fixes all degrees of freedom to the base reference frame
        '==========================================================================================
        Public Sub Clamped()

            For i = 0 To 5
                Fixed(i) = True
            Next

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
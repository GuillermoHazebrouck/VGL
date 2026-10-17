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

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.AeroTools.Models.Structural.Library.Nodes

'#############################################################################
' Unit: Modal
'
' This unit provides a representation of modal shapes.
'#############################################################################
Namespace AeroTools.Models.Structural.Library

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a modal shape.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Mode

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The displacements of the shape at each node.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Shape As List(Of NodalDisplacement)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The angular frequency
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public W As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The modal stiffness.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public K As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The modal mass.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public M As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The modal damping (absolute)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public C As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The critical modal damping
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Cc As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The mode index
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Index As Integer

        '==========================================================================================
        ' Generates a new mode with the given index.
        ' Index is the index of the mode in the external stack.
        '==========================================================================================
        Public Sub New(ByVal Index As Integer)
            Shape = New List(Of NodalDisplacement)
            _Index = Index
        End Sub
        '------------------------------------------------------------------------------------------

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents an instantaneous state of the modal shape.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class ModalCoordinate

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Position
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public P As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Velocity
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public V As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Acceleration
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public A As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The virtual work
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public W As Double

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a set of modal coordinates (one for each mode at a given time step).
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class ModalCoordinates

        Private _ModalCoordinates As List(Of ModalCoordinate)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Modal coordinates for a given mode.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Default Public Property Item(ByVal Index As Integer) As ModalCoordinate
            Get
                Return _ModalCoordinates(Index)
            End Get
            Set(ByVal value As ModalCoordinate)
                _ModalCoordinates(Index) = value
            End Set
        End Property

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Returns the size of the stack (the number of tracked modes).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Count As Integer
            Get
                Return _ModalCoordinates.Count
            End Get
        End Property

        '==========================================================================================
        ' Generates a stack of modal coordinates (one for each mode)
        ' Size is the number of modes.
        '==========================================================================================
        Public Sub New(ByVal Size As Integer)

            _ModalCoordinates = New List(Of ModalCoordinate)(Size)

            For I = 1 To Size
                _ModalCoordinates.Add(New ModalCoordinate)
            Next

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
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
' Unit: Selection
'
' This unit provides means for general object selection
'#############################################################################
Namespace DesignTools.Interface

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The different types of components that can be interacted with.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum ComponentTypes As Integer

        etNothing = 0
        etLiftingSurface = 1
        etFuselage = 2
        etResultContainer = 3
        etWake = 4
        etJetEngine = 5
        etPropeller = 6

    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The different kind of entities that can be interacted with.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum EntityTypes As Integer

        etNothing = 0
        etNode = 1
        etSegment = 2
        etPanel = 3
        etStructuralElement = 4
        etStructuralNode = 5

    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Provides information about a selection based in a single integer identifier.
    ' Note that the selection mechanism is not part of this library.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Structure SelectionInfo

        Private _Id As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads back the code and sets identity credentials.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Id As Integer
            Set(ByVal value As Integer)
                _Id = value
                SetUp()
            End Set
            Get
                Return _Id
            End Get
        End Property

        Private _ComponentType As ComponentTypes

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The component type given as described by the ComponentTypes enum.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property ComponentType As ComponentTypes
            Get
                Return _ComponentType
            End Get
        End Property

        Private _EntityType As EntityTypes

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The entity type given as described by the EntityTypes enum.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property EntityType As EntityTypes
            Get
                Return _EntityType
            End Get
        End Property

        Private _ComponentIndex As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The component index in its original stack.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property ComponentIndex As Integer
            Get
                Return _ComponentIndex
            End Get
        End Property

        Private _EntityIndex As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The entity index in its original stack inside the parent component.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property EntityIndex As Integer
            Get
                Return _EntityIndex
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Decodes the identifier as a component type, component index, entity type and entity index.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private Sub SetUp()

            Dim Name As String = _Id.ToString

            _ComponentType = CInt(Left(Name, 1))

            _ComponentIndex = CInt(Mid(Name, 2, 2))

            _EntityType = Mid(Name, 4, 1)

            _EntityIndex = CInt(Mid(Name, 5, 4))

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The depth of the entity associated to this selection info.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <returns></returns>
        Public Property EyeDepth As Double

    End Structure

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Gathers a collection of selection infos.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Selection

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The entities that have be selected.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public SelectionList As New List(Of SelectionInfo)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The specific entities that can be selected.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public EntityToSelect As EntityTypes = EntityTypes.etPanel

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if multiple entities can be selected at the same time.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public MultipleSelection As Boolean = False

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Returns the selection Id for the given component and entity.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Shared Function GetSelectionCode(ByVal ElementType As ComponentTypes, ByVal ElementIndex As Integer, ByVal EntityType As EntityTypes, ByVal EntityIndex As Integer) As Integer
            Return ElementType * 10000000 + ElementIndex * 100000 + EntityType * 10000 + EntityIndex
        End Function

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Interface that must be implemented by components that can be selected.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Interface ISelectable

        Property Active As Boolean
        Sub UnselectAll()

    End Interface

End Namespace

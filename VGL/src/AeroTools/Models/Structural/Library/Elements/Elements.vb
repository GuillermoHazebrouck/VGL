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
Imports DotNumerics.LinearAlgebra

'#############################################################################
' Unit: Elements
'
' This unit provides a generic interface for finite elements
'#############################################################################
Namespace AeroTools.Models.Structural.Library.Elements

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The interface of a generic finite element used for structural analisys.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Interface IFiniteElement

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The nodes of the element.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property Nodes As StructuralNode()

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The mass matrix
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property M As SymmetricMatrix

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The stiffness matrix
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property K As SymmetricMatrix

        Sub GenerateLocalMass()
        Sub GenerateLocalStiffness()
        Sub GenerateGlobalMatrices()

    End Interface
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------

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
' Unit: Material
'
' This unit is out of service, since the section properties already include
' the material properies.
' This unit might be removed in the future.
'#############################################################################
Namespace AeroTools.Models.Structural.Library

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a material.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Material

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Young module [N/m²]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public E As Double ' 210GPa

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Transverse module [N/m²]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public G As Double ' 100GPa

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Poisson coefficient
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public v As Double = 0.3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Material density [kg/m³]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Density As Double = 7800

        '==========================================================================================
        '
        '==========================================================================================
        Public Sub New()
            E = 69000000000 ' Aluminium
            v = 0.3
            G = 0.5 * E / (1 + v)
        End Sub
        '------------------------------------------------------------------------------------------

        '==========================================================================================
        '
        '==========================================================================================
        Public Sub Assign(ByVal Material As Material)

            E = Material.E
            G = Material.G
            v = Material.v
            Density = Material.Density

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
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
Imports VGL.AeroTools.Models.Structural.Library.Elements
Imports VGL.MathTools.Algebra.EuclideanSpace

'#############################################################################
' Unit: StructuralPartition
'
' This unit provides a generic structural partition for computing the
' structural properties of wing sections.
'#############################################################################
Namespace DesignTools.Models.Components.Basics

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents an structural partition
    '//////////////////////////////////////////////////////////////////////////////////////////
    ' <remarks></remarks>
    Public Class StructuralPartition

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Position of this partition
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public P As Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The center of mass
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public M As Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The local system of coordinates
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Basis As Base3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Section associated to this partition point
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public LocalSection As Section

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Chord associated to this partition
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public LocalChord As Double

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Creates a new partition with the given position
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <param name="p"></param>
        ' <param name="section"></param>
        Public Sub New(ByVal p As Vector3, ByVal section As Section)

            Me.P = New Vector3(p.X, p.Y, p.Z)
            Me.M = New Vector3(p.X, p.Y, p.Z)

            LocalSection = New Section
            LocalSection.Assign(section)

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Creates a new partition
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New()

            Me.P = New Vector3()
            Me.M = New Vector3()

            LocalSection = New Section
            Basis = New Base3()

        End Sub
        '------------------------------------------------------------------------------------------

    End Class

End Namespace


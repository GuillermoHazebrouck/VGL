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

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.MathTools.Magnitudes

'#############################################################################
' Unit: GlobalMagnitudes
'
' This unit provides the global magnitudes (not used yet).
'#############################################################################
Namespace DesignTools.DataStore

    Public Module GlobalMagnitudes

        Public UserMagnitudes As New Dictionary(Of Magnitudes, IPhysicalMagnitude)

        Public GlobalDecimals As New Dictionary(Of Magnitudes, Integer)

        Sub New()

            UserMagnitudes.Clear()

            Dim DeclaredUnits As Array
            DeclaredUnits = [Enum].GetValues(GetType(Magnitudes))

            For Each UnitType As Magnitudes In DeclaredUnits

                UserMagnitudes.Add(UnitType, Units.GetInstanceOf(UnitType))
                GlobalDecimals.Add(UnitType, 3)

            Next

            GlobalDecimals(Magnitudes.Force) = 0
            GlobalDecimals(Magnitudes.Moment) = 0
            GlobalDecimals(Magnitudes.Dimensionless) = 5
            GlobalDecimals(Magnitudes.Density) = 4
            GlobalDecimals(Magnitudes.Temperature) = 1
            GlobalDecimals(Magnitudes.Pressure) = 0

        End Sub

    End Module

End Namespace

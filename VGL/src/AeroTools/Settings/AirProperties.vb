'#############################################################################
'VGL
'Open source aeromechanics in dotnet
'Copyright (C) 2026 George Lazarou (george.sp.lazarou@gmail.com)

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

' This unit has been incorporated by Georgios Lazarou.

'#############################################################################
' Unit: AirProperties
'
' This unit provides model of the standard atmosphere
'#############################################################################
Namespace AeroTools.Settings

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The standard atmosphere
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class StandardAtmosphere

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The temperature [K]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Temperature As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The static pressure [Pa]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Pressure As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The density [kg/m³]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Density As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The dynamic viscosity [N/m.s]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property DynamicVisc As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '  The dynamic viscosity [m²/s]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property KinematicVisc As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The speed of sound [m/s]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property SoundSpeed As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The altitude [m]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Altitude As Double



        '==========================================================================================
        ' Generates the properties at sea level (altitude = 0m).
        '==========================================================================================
        Public Sub New()
            CalculateAirProperties(0.0#)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Generates the properties at the given altitude
        '==========================================================================================
        Public Sub New(Altitude As Double)
            CalculateAirProperties(Altitude)
        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Calculates the properties for a given altitude
        '==========================================================================================
        Public Sub CalculateAirProperties(Altitude As Double)

            _Altitude = Altitude
            _Temperature = 288.16 - 0.0065 * Altitude 'Unit: [K]
            _Pressure = 101325 * (1 - 2.25569 * 10 ^ (-5) * Altitude) ^ 5.25616 'Unit: [Pa]
            _Density = 1.225 * (_Temperature / 288.16) ^ 4.2561 'Unit: [kg/m3]
            _DynamicVisc = (1.458 * 10 ^ (-6) * _Temperature ^ 1.5) / (_Temperature + 110.4) 'Unit: [kg/m*s]
            _KinematicVisc = _DynamicVisc / _Density 'Unit: [m2/s]
            _SoundSpeed = Math.Sqrt(401.874018 * _Temperature) 'Unit: [m/s]

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------
    
End Namespace
'--------------------------------------------------------------------------------------------------
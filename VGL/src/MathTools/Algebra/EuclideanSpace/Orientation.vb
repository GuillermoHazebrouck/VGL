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

Namespace MathTools.Algebra.EuclideanSpace

    '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ' The available rotation sequences.
    ' Other secuences can be added here, but you have to add the transformation matrix.
    '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Public Enum RotationSequence As Byte

        'Euler sequence
        ZYX = 0

        'Tait-Bryan sequence (yaw/pitch/roll)
        XYZ = 1

    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents an orientation in Euler angles
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class OrientationAngles

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Pi divided by 180.0#
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Const PiDiv180 As Double = Math.PI / 180.0#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Yaw angle
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Angle1 As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Pitch angle
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Angle2 As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Roll angle
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Angle3 As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The rotation sequence
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Sequence As RotationSequence = RotationSequence.ZYX

        '=========================================================================================
        ' Resets the orientation to zero.
        '=========================================================================================
        Public Sub SetToZero()
            Angle1 = 0
            Angle2 = 0
            Angle3 = 0
        End Sub
        '------------------------------------------------------------------------------------------



        '=========================================================================================
        ' Converts the angles to radians (assuming they are in degrees)
        '=========================================================================================
        Public Sub ToRadians()

            Angle1 *= PiDiv180
            Angle2 *= PiDiv180
            Angle3 *= PiDiv180

        End Sub
        '------------------------------------------------------------------------------------------



        '=========================================================================================
        ' Converts the angles to radians (assuming they are in degrees)
        '=========================================================================================
        Public Function InRadians() As OrientationAngles

            Dim NewOrientation As New OrientationAngles
            NewOrientation.Assign(Me)
            NewOrientation.ToRadians()
            Return NewOrientation

        End Function

        '=========================================================================================
        ' Converts the angles to degrees (assuming they are in radians)
        '=========================================================================================
        Public Sub ToDegrees()

            Angle1 /= PiDiv180
            Angle2 /= PiDiv180
            Angle3 /= PiDiv180

        End Sub
        '------------------------------------------------------------------------------------------



        '=========================================================================================
        ' Assigns the same value.
        '=========================================================================================
        Public Sub Assign(ByVal Orientation As OrientationAngles)

            Angle1 = Orientation.Angle1
            Angle2 = Orientation.Angle2
            Angle3 = Orientation.Angle3

            Sequence = Orientation.Sequence

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
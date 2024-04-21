﻿'VGL
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

Namespace MathTools.Algebra.EuclideanSpace

    ''' <summary>
    ''' The available rotation sequences.
    ''' Other secuences can be added here, but you have to add the transformation matrix.
    ''' </summary>
    Public Enum RotationSequence As Byte

        'Euler sequence
        ZYX = 0

        'Tait-Bryan sequence (yaw/pitch/roll)
        XYZ = 1

    End Enum

    ''' <summary>
    ''' Represents an orientation in Euler angles
    ''' </summary>
    ''' <remarks></remarks>
    Public Class OrientationAngles

        ''' <summary>
        ''' Pi divided by 180.0#
        ''' </summary>
        Public Const PiDiv180 As Double = Math.PI / 180.0#

        ''' <summary>
        ''' Yaw angle
        ''' </summary>
        Public Property Angle1 As Double

        ''' <summary>
        ''' Pitch angle
        ''' </summary>
        Public Property Angle2 As Double

        ''' <summary>
        ''' Roll angle
        ''' </summary>
        Public Property Angle3 As Double

        ''' <summary>
        ''' The rotation sequence
        ''' </summary>
        ''' <returns></returns>
        Public Property Sequence As RotationSequence = RotationSequence.ZYX

        ''' <summary>
        ''' Resets the orientation to zero.
        ''' </summary>
        Public Sub SetToZero()
            Angle1 = 0
            Angle2 = 0
            Angle3 = 0
        End Sub

        ''' <summary>
        ''' Converts the angles to radians (assuming they are in degrees)
        ''' </summary>
        ''' <returns></returns>
        Public Sub ToRadians()

            Angle1 *= PiDiv180
            Angle2 *= PiDiv180
            Angle3 *= PiDiv180

        End Sub

        ''' <summary>
        ''' Converts the angles to radians (assuming they are in degrees)
        ''' </summary>
        ''' <returns></returns>
        Public Function InRadians() As OrientationAngles

            Dim NewOrientation As New OrientationAngles
            NewOrientation.Assign(Me)
            NewOrientation.ToRadians()
            Return NewOrientation

        End Function

        ''' <summary>
        ''' Converts the angles to degrees (assuming they are in radians)
        ''' </summary>
        ''' <returns></returns>
        Public Sub ToDegrees()

            Angle1 /= PiDiv180
            Angle2 /= PiDiv180
            Angle3 /= PiDiv180

        End Sub

        ''' <summary>
        ''' Assigns the same value.
        ''' </summary>
        ''' <param name="Orientation"></param>
        Public Sub Assign(ByVal Orientation As OrientationAngles)

            Angle1 = Orientation.Angle1
            Angle2 = Orientation.Angle2
            Angle3 = Orientation.Angle3

            Sequence = Orientation.Sequence

        End Sub

    End Class

End Namespace

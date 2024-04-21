﻿'#############################################################################
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
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports DotNumerics.LinearAlgebra

'#############################################################################
' Unit: InertialProperties
'
' This unit provides a definition of inertial properties, mainly used for the
' rigid body simulations.
' Operators are provided to easly sum the inertial properties of the model
' components.
' Main axes of inertia are calculated through an eigen values problem.
'#############################################################################
Namespace AeroTools.Settings

    ''' <summary>
    ''' Represent the inertial properties of a body.
    ''' The axes are assumed to cross the body CG.
    ''' </summary>
    Public Structure InertialProperties

        ''' <summary>
        ''' Center of uniform force field.
        ''' </summary>
        ''' <returns></returns>
        Public Property Xcg As Double

        ''' <summary>
        ''' Center of uniform force field.
        ''' </summary>
        ''' <returns></returns>
        Public Property Ycg As Double

        ''' <summary>
        ''' Center of uniform force field.
        ''' </summary>
        ''' <returns></returns>
        Public Property Zcg As Double

        ''' <summary>
        ''' The mass of the system.
        ''' </summary>
        ''' <returns></returns>
        Public Property Mass As Double

        ''' <summary>
        ''' Moment of ineratia about the X axis.
        ''' </summary>
        ''' <returns></returns>
        Public Property Ixx As Double

        ''' <summary>
        ''' Moment of ineratia about the Y axis.
        ''' </summary>
        ''' <returns></returns>
        Public Property Iyy As Double

        ''' <summary>
        ''' Moment of ineratia about the Z axis.
        ''' </summary>
        ''' <returns></returns>
        Public Property Izz As Double

        ''' <summary>
        ''' Cross moment of ineratia about the XY axes.
        ''' </summary>
        ''' <returns></returns>
        Public Property Ixy As Double

        ''' <summary>
        ''' Cross moment of ineratia about the XZ axes.
        ''' </summary>
        ''' <returns></returns>
        Public Property Ixz As Double

        ''' <summary>
        ''' Cross moment of ineratia about the YZ axes.
        ''' </summary>
        ''' <returns></returns>
        Public Property Iyz As Double

        ''' <summary>
        ''' Adds the two inertial properties using the transport theorem.
        ''' </summary>
        ''' <param name="I1"></param>
        ''' <param name="I2"></param>
        ''' <returns></returns>
        Public Shared Operator +(I1 As InertialProperties, I2 As InertialProperties)

            Dim I As InertialProperties

            I.Mass = I1.Mass + I2.Mass

            If I.Mass > 0.0# Then
                I.Xcg = (I1.Xcg * I1.Mass + I2.Xcg * I2.Mass) / I.Mass
                I.Ycg = (I1.Ycg * I1.Mass + I2.Ycg * I2.Mass) / I.Mass
                I.Zcg = (I1.Zcg * I1.Mass + I2.Zcg * I2.Mass) / I.Mass
            Else
                I.Xcg = 0.0#
                I.Ycg = 0.0#
                I.Zcg = 0.0#
            End If

            Dim I1_Xcg As Double = I1.Xcg - I.Xcg
            Dim I1_Ycg As Double = I1.Ycg - I.Ycg
            Dim I1_Zcg As Double = I1.Zcg - I.Zcg

            Dim I2_Xcg As Double = I2.Xcg - I.Xcg
            Dim I2_Ycg As Double = I2.Ycg - I.Ycg
            Dim I2_Zcg As Double = I2.Zcg - I.Zcg

            Dim I1_Ixx As Double = (I1_Ycg ^ 2 + I1_Zcg ^ 2) * I1.Mass
            Dim I2_Ixx As Double = (I2_Ycg ^ 2 + I2_Zcg ^ 2) * I2.Mass
            I.Ixx = I1.Ixx + I2.Ixx + I1_Ixx + I2_Ixx

            Dim I1_Iyy As Double = (I1_Xcg ^ 2 + I1.Zcg ^ 2) * I1.Mass
            Dim I2_Iyy As Double = (I2_Xcg ^ 2 + I2_Zcg ^ 2) * I2.Mass
            I.Iyy = I1.Iyy + I2.Iyy + I1_Iyy + I2_Iyy

            Dim I1_Izz As Double = (I1_Xcg ^ 2 + I1_Ycg ^ 2) * I1.Mass
            Dim I2_Izz As Double = (I2_Xcg ^ 2 + I2_Ycg ^ 2) * I2.Mass
            I.Izz = I1.Izz + I2.Izz + I1_Izz + I2_Izz

            Dim I1_Ixy As Double = I1_Xcg * I1_Ycg * I1.Mass
            Dim I2_Ixy As Double = I2_Xcg * I2_Ycg * I2.Mass
            I.Ixy = I1.Ixy + I2.Ixy + I1_Ixy + I2_Ixy

            Dim I1_Ixz As Double = I1_Xcg * I1_Zcg * I1.Mass
            Dim I2_Ixz As Double = I2_Xcg * I2_Zcg * I2.Mass
            I.Ixz = I1.Ixz + I2.Ixz + I1_Ixz + I2_Ixz

            Dim I1_Iyz As Double = I1_Ycg * I1_Zcg * I1.Mass
            Dim I2_Iyz As Double = I2_Ycg * I2_Zcg * I2.Mass
            I.Iyz = I1.Iyz + I2.Iyz + I1_Iyz + I2_Iyz

            Return I

        End Operator

        ''' <summary>
        ''' Transforms the inertial properties to the given axes. This also will align the CG 
        ''' coordinates and add the CG offset.
        ''' </summary>
        ''' <param name="Inertia"></param>
        ''' <param name="Basis"></param>
        Public Sub Transform(Offset As Vector3, ByVal Basis As Base3)

            Dim X As Double = Xcg
            Dim Y As Double = Ycg
            Dim Z As Double = Zcg

            Xcg = Offset.X + X * Basis.U.X + Y * Basis.U.Y + Z * Basis.U.Z
            Ycg = Offset.Y + X * Basis.V.X + Y * Basis.V.Y + Z * Basis.V.Z
            Zcg = Offset.Z + X * Basis.W.X + Y * Basis.W.Y + Z * Basis.W.Z

            Dim I As InertialProperties

            ' Premultiply

            I.Ixx = Ixx * Basis.U.X + Ixy * Basis.V.X + Ixz * Basis.W.X
            I.Ixy = Ixx * Basis.U.Y + Ixy * Basis.V.Y + Ixz * Basis.W.Y
            I.Ixz = Ixx * Basis.U.Z + Ixy * Basis.V.Z + Ixz * Basis.W.Z

            I.Iyy = Ixy * Basis.U.Y + Iyy * Basis.V.Y + Iyz * Basis.W.Y
            I.Iyz = Ixy * Basis.U.Z + Iyy * Basis.V.Z + Iyz * Basis.W.Z

            I.Izz = Ixz * Basis.U.Z + Ixz * Basis.V.Z + Izz * Basis.W.Z

            ' Posmultiply

            Ixx = I.Ixx * Basis.U.X + I.Ixy * Basis.U.Y + I.Ixz * Basis.U.Z
            Ixy = I.Ixx * Basis.V.X + I.Ixy * Basis.V.Y + I.Ixz * Basis.V.Z
            Ixz = I.Ixx * Basis.W.X + I.Ixy * Basis.W.Y + I.Ixz * Basis.W.Z

            Iyy = I.Ixy * Basis.V.X + I.Iyy * Basis.V.Y + I.Iyz * Basis.V.Z
            Iyz = I.Ixy * Basis.W.X + I.Iyy * Basis.W.Y + I.Iyz * Basis.W.Z

            Izz = I.Ixz * Basis.W.X + I.Ixz * Basis.W.Y + I.Izz * Basis.W.Z

        End Sub

        ''' <summary>
        ''' Convertes the general tensor of inertia to a the main axes of inertia
        ''' </summary>
        ''' <param name="Basis"></param>
        ''' <param name="I_xx"></param>
        ''' <param name="I_yy"></param>
        ''' <param name="I_zz"></param>
        Public Sub ToMainInertia(ByRef Basis As Base3,
                                 ByRef I_xx As Double,
                                 ByRef I_yy As Double,
                                 ByRef I_zz As Double)

            ' Return the standard basis for singular inertia
            '-----------------------------------------------

            If Ixx = 0 Or Iyy = 0 Or Izz = 0 Then

                Basis.U.X = 1.0#
                Basis.U.Y = 0.0#
                Basis.U.Z = 0.0#

                Basis.V.X = 0.0#
                Basis.V.Y = 1.0#
                Basis.V.Z = 0.0#

                Basis.W.X = 0.0#
                Basis.W.Y = 0.0#
                Basis.W.Z = 1.0#

                Return

            End If

            ' Solve the eigen system
            '-----------------------------------------------

            Dim E As New EigenSystem()
            Dim M As New SymmetricMatrix(3)

            M(0, 0) = Ixx
            M(0, 1) = Ixy
            M(0, 2) = Ixz
            M(1, 1) = Iyy
            M(1, 2) = Iyz
            M(2, 2) = Izz

            ' Solve the eigen system
            '--------------------------------------

            Dim V As New Matrix(3)
            Dim A As Matrix = E.GetEigenvalues(M, V)

            I_xx = A(0, 0)
            I_yy = A(1, 0)
            I_zz = A(2, 0)

            Basis.U.X = V(0, 0)
            Basis.U.Y = V(1, 0)
            Basis.U.Z = V(2, 0)

            Basis.V.X = V(0, 1)
            Basis.V.Y = V(1, 1)
            Basis.V.Z = V(2, 1)

            ' Force the basis to be dextro-rotation
            '--------------------------------------

            Basis.W.FromVectorProduct(Basis.U, Basis.V)

        End Sub

        Public Sub SetToZero()

            Mass = 0.0#
            Xcg = 0.0#
            Ycg = 0.0#
            Zcg = 0.0#

            Ixx = 0.0#
            Iyy = 0.0#
            Izz = 0.0#

            Ixy = 0.0#
            Ixz = 0.0#
            Iyz = 0.0#

        End Sub

    End Structure

End Namespace


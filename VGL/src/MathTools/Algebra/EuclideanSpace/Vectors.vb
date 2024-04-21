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

Imports VGL.MathTools.Algebra.CustomMatrices

Namespace MathTools.Algebra.EuclideanSpace

    ''' <summary>
    ''' Represents an euclidean vector of two coordinates.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Vector2

        Public Property X As Double
        Public Property Y As Double

        Public Sub New()

        End Sub

        Public Sub New(ByVal x As Double, ByVal y As Double)
            Me.X = x
            Me.Y = y
        End Sub

#Region " Operaciones aritméticos "

        Public Shared Operator +(ByVal V1 As Vector2, ByVal V2 As Vector2) As Vector2
            Dim Suma As New Vector2
            Suma.X = V1.X + V2.X
            Suma.Y = V1.Y + V2.Y
            Return Suma
        End Operator

        Public Shared Operator -(ByVal V1 As Vector2, ByVal V2 As Vector2) As Vector2
            Dim Resta As New Vector2
            Resta.X = V1.X - V2.X
            Resta.Y = V1.Y - V2.Y
            Return Resta
        End Operator

        Public Shared Operator -(ByVal V As Vector2) As Vector2
            Dim Opuesto As New Vector2
            Opuesto.X = -V.X
            Opuesto.Y = -V.Y
            Return Opuesto
        End Operator

        Public Overloads Shared Operator *(ByVal V1 As Vector2, ByVal V2 As Vector2) As Double
            Return V1.X * V2.X + V1.Y * V2.Y
        End Operator

        Public Overloads Shared Operator *(ByVal Escalar As Double, ByVal V As Vector2) As Vector2
            Dim Producto As New Vector2

            Producto.X = Escalar * V.X
            Producto.Y = Escalar * V.Y

            Return Producto
        End Operator

        Public Shared Operator ^(ByVal V1 As Vector2, ByVal V2 As Vector2) As Double

            Return V1.X * V2.Y - V1.Y * V2.X

        End Operator

        Public Sub Oppose()
            Me.X = -Me.X
            Me.Y = -Me.Y
        End Sub

        Public Sub Scale(ByVal Scalar As Double)
            Me.X = Scalar * Me.X
            Me.Y = Scalar * Me.Y
        End Sub

        Public Function PinCrossProduct(ByVal V1 As Vector2, ByVal V2 As Vector2) As Double

            Dim x1 As Double = V1.X - X
            Dim y1 As Double = V1.Y - Y

            Dim x2 As Double = V2.X - X
            Dim y2 As Double = V2.Y - Y

            Return x1 * y2 - y1 * x2

        End Function

#End Region

#Region " Metric operations "

        Public ReadOnly Property Norm2 As Double
            Get
                Return Math.Sqrt(Me * Me)
            End Get
        End Property

        Public ReadOnly Property SqrNorm2 As Double
            Get
                Return X * X + Y * Y
            End Get
        End Property

        Public Function RelativePosition(ByVal Punto As Vector2) As Vector2
            Return New Vector2(Punto.X - X, Punto.Y - Y)
        End Function

        Public Function DistanceTo(ByVal Punto As Vector2) As Double
            Dim dx As Double = X - Punto.X
            Dim dy As Double = Y - Punto.Y
            Return Math.Sqrt(dx * dx + dy * dy)
        End Function

        Public ReadOnly Property ProjectionVector(ByVal Vector As Vector2) As Vector2
            Get
                Return (Me * Vector) / (Vector * Vector) * Vector
            End Get
        End Property

        Public ReadOnly Property OrthogonalVector(ByVal Vector As Vector2) As Vector2
            Get
                Return Me - Me.ProjectionVector(Vector)
            End Get
        End Property

        Public Sub Ortogonalize()

            Dim Xo As Double = Me.X

            Me.X = -Me.Y
            Me.Y = Xo

        End Sub

        Public Sub Normalize()
            Dim Norm As Double = Me.Norm2
            Me.X = Me.X / Norm
            Me.Y = Me.Y / Norm
        End Sub

        Public Function DotProduct(ByVal v As Vector2) As Double
            Return X * v.X + Y * v.Y
        End Function

#End Region

#Region " Otras operaciones "

        Public Sub SetToCero()
            Me.X = 0
            Me.Y = 0
        End Sub

        Public Sub ReadFromString(ByVal Line As String, Optional ByVal Margen As Integer = 1)
            Me.X = CDbl(Mid(Line, Margen, 25))
            Me.Y = CDbl(Mid(Line, Margen + 25, 25))
        End Sub

        Public Sub SetCoordinates(ByVal X As Double, ByVal Y As Double)
            Me.X = X
            Me.Y = Y
        End Sub

        ''' <summary>
        ''' Rotates the vector an angle a [rad]
        ''' </summary>
        ''' <param name="a"></param>
        ''' <remarks></remarks>
        Public Overloads Sub Rotate(ByVal a As Double)

            Dim rotX = X * Math.Cos(a) - Y * Math.Sin(a)
            Dim rotY = X * Math.Sin(a) + Y * Math.Cos(a)
            Me.X = rotX
            Me.Y = rotY

        End Sub

        ''' <summary>
        ''' Rotates the vector an angle a [rad]
        ''' </summary>
        ''' <param name="a"></param>
        ''' <remarks></remarks>
        Public Overloads Sub Rotate(ByVal a As Double, ByVal x As Double, ByVal y As Double)
            Me.X -= x
            Me.Y -= y
            Dim rotX As Double = Me.X * Math.Cos(a) - Me.Y * Math.Sin(a)
            Dim rotY As Double = Me.X * Math.Sin(a) + Me.Y * Math.Cos(a)
            Me.X = rotX + x
            Me.Y = rotY + y
        End Sub

        Public Shared Function ParametricIntersection(ByVal p1 As Vector2, ByVal v1 As Vector2, ByVal p2 As Vector2, ByVal v2 As Vector2) As Vector2

            Dim det As Double = v2.X * v1.Y - v1.X * v2.Y

            If det <> 0 Then

                Dim intersection As New Vector2
                Dim bx As Double = p2.X - p1.X
                Dim by As Double = p2.Y - p1.Y

                intersection.X = (-v2.Y * bx + v2.X * by) / det
                intersection.Y = (-v1.Y * bx + v1.X * by) / det

                Return intersection

            Else

                Return Nothing

            End If

        End Function

        ''' <summary>
        ''' Calculates the intersection between line {p, v} with segment {a, b} as a coordinate relative to point a.
        ''' </summary>
        ''' <param name="p"></param>
        ''' <param name="v"></param>
        ''' <param name="a"></param>
        ''' <param name="b"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Shared Function IntersectionCoordinate(ByVal p As Vector2, ByVal v As Vector2, ByVal a As Vector2, ByVal b As Vector2) As Double

            Dim u As New Vector2(b.X - a.X, b.Y - a.Y)
            Dim coordinates As Vector2 = ParametricIntersection(p, v, a, u)

            If IsNothing(coordinates) Then

                Return Double.NaN

            Else

                If coordinates.Y >= 0 And coordinates.Y <= 1 Then
                    Return coordinates.Y
                Else
                    Return Double.NaN
                End If

            End If

        End Function

#End Region

    End Class

    ''' <summary>
    ''' Represents an euclidean vector of three coordinates.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Vector3

        Public X As Double
        Public Y As Double
        Public Z As Double
        Public Active As Boolean = False

        Public Sub New()

        End Sub

        Public Sub New(ByVal Vector As Vector3)
            Me.X = Vector.X
            Me.Y = Vector.Y
            Me.Z = Vector.Z
        End Sub

        Public Sub New(ByVal Vector As Vector3, ByVal Factor As Double)
            Me.X = Factor * Vector.X
            Me.Y = Factor * Vector.Y
            Me.Z = Factor * Vector.Z
        End Sub

        Public Sub New(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)
            Me.X = X
            Me.Y = Y
            Me.Z = Z
        End Sub

        Public Overrides Function ToString() As String

            Return String.Format("{0:F14}, {1:F14}, {2:F14}", X, Y, Z)

        End Function

#Region " Operaciones aritméticas "

        ''' <summary>
        ''' Adds the given vector
        ''' </summary>
        ''' <param name="Vector"></param>
        ''' <remarks></remarks>
        Public Sub Add(ByVal Vector As Vector3)
            Me.X += Vector.X
            Me.Y += Vector.Y
            Me.Z += Vector.Z
        End Sub

        ''' <summary>
        ''' Adds the given vector premltiplied by the given scalar
        ''' </summary>
        ''' <param name="Vector"></param>
        ''' <param name="Scalar"></param>
        ''' <remarks></remarks>
        Public Sub Add(ByVal Vector As Vector3, ByVal Scalar As Double)

            Me.X += Vector.X * Scalar
            Me.Y += Vector.Y * Scalar
            Me.Z += Vector.Z * Scalar

        End Sub

        ''' <summary>
        ''' Adds the given vector coordinates
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub Add(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)
            Me.X += X
            Me.Y += Y
            Me.Z += Z
        End Sub

        ''' <summary>
        ''' Substracts the given vector
        ''' </summary>
        ''' <param name="Vector"></param>
        ''' <remarks></remarks>
        Public Sub Substract(ByVal Vector As Vector3)

            Me.X -= Vector.X
            Me.Y -= Vector.Y
            Me.Z -= Vector.Z

        End Sub

        ''' <summary>
        ''' Adds the cross product between two vectors
        ''' </summary>
        ''' <param name="V1"></param>
        ''' <param name="V2"></param>
        ''' <remarks></remarks>
        Public Sub AddCrossProduct(ByVal V1 As Vector3, ByVal V2 As Vector3)
            X += V1.Y * V2.Z - V1.Z * V2.Y
            Y += V1.Z * V2.X - V1.X * V2.Z
            Z += V1.X * V2.Y - V1.Y * V2.X
        End Sub

        ''' <summary>
        ''' Adds the cross product between two vectors multiplied by a scalar
        ''' </summary>
        ''' <param name="V1"></param>
        ''' <param name="V2"></param>
        ''' <remarks></remarks>
        Public Sub AddCrossProduct(ByVal V1 As Vector3, ByVal V2 As Vector3, Scalar As Double)
            X += Scalar * (V1.Y * V2.Z - V1.Z * V2.Y)
            Y += Scalar * (V1.Z * V2.X - V1.X * V2.Z)
            Z += Scalar * (V1.X * V2.Y - V1.Y * V2.X)
        End Sub

        Public Shared Operator +(ByVal V1 As Vector3, ByVal V2 As Vector3) As Vector3
            Dim Suma As New Vector3
            Suma.X = V1.X + V2.X
            Suma.Y = V1.Y + V2.Y
            Suma.Z = V1.Z + V2.Z
            Return Suma
        End Operator

        Public Shared Operator -(ByVal V1 As Vector3, ByVal V2 As Vector3) As Vector3
            Dim Resta As New Vector3
            Resta.X = V1.X - V2.X
            Resta.Y = V1.Y - V2.Y
            Resta.Z = V1.Z - V2.Z
            Return Resta
        End Operator

        Public Shared Operator -(ByVal V As Vector3) As Vector3
            Dim Opuesto As New Vector3
            Opuesto.X = -V.X
            Opuesto.Y = -V.Y
            Opuesto.Z = -V.Z
            Return Opuesto
        End Operator

        Public Overloads Shared Operator *(ByVal V1 As Vector3, ByVal V2 As Vector3) As Double
            Return V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z
        End Operator

        Public Overloads Shared Operator *(ByVal Escalar As Double, ByVal V As Vector3) As Vector3
            Dim Producto As New Vector3

            Producto.X = Escalar * V.X
            Producto.Y = Escalar * V.Y
            Producto.Z = Escalar * V.Z

            Return Producto
        End Operator

        Public Shared Operator ^(ByVal V1 As Vector3, ByVal V2 As Vector3) As Vector3
            Dim Producto As New Vector3

            Producto.X = V1.Y * V2.Z - V1.Z * V2.Y
            Producto.Y = V1.Z * V2.X - V1.X * V2.Z
            Producto.Z = V1.X * V2.Y - V1.Y * V2.X

            Return Producto
        End Operator

        Public Sub Oppose()
            Me.X = -Me.X
            Me.Y = -Me.Y
            Me.Z = -Me.Z
        End Sub

#End Region

#Region " Operaciones métricas "

        Public Function InnerProduct(ByVal Punto As Vector3) As Double
            Return X * Punto.X + Y * Punto.Y + Z * Punto.Z
        End Function

        Public ReadOnly Property Norm2 As Double
            Get
                ' In the .NET frameworks X * X is much more effective than X ^ 2.
                Dim Value As Double = Math.Sqrt(X * X + Y * Y + Z * Z)
                Return Value
            End Get
        End Property

        Public ReadOnly Property SquareEuclideanNorm As Double
            Get
                Dim Value As Double = X * X + Y * Y + Z * Z
                Return Value
            End Get
        End Property

        ' Review these properties. They might work much slower than they should!!

        Public Function RelativePosition(ByVal Punto As Vector3) As Vector3
            Dim Posicion As New Vector3(Punto)
            Posicion.Substract(Me)
            Return Posicion
        End Function

        Public Function DistanceTo(ByVal Punto As Vector3) As Double
            Return Me.RelativePosition(Punto).Norm2
        End Function

        Public ReadOnly Property ProjectedVector(ByVal Vector As Vector3) As Vector3
            Get
                Dim Projection As Double = Algebra.InnerProduct(Me, Vector)
                Dim SqrNorm As Double = Vector.SquareEuclideanNorm
                Return Algebra.ScalarProduct(Projection / SqrNorm, Vector)
            End Get
        End Property

        Public ReadOnly Property OrthogonalVector(ByVal Vector As Vector3) As Vector3
            Get
                Dim Projection As Vector3 = ProjectedVector(Vector)
                Return Algebra.SubstractVectors(Me, Projection)
            End Get
        End Property

        Public ReadOnly Property NormalizedDirection As Vector3
            Get
                Dim Norma As Double = Me.Norm2
                Dim VectorEscalado As New Vector3(Me, 1 / Norma)
                'VectorEscalado.Escalar(1 / Norma)
                Return VectorEscalado
                'Dim Nm1 As Double = 1 / Me.NormaEuclidea
                'Return New EVector3(Nm1 * X, Nm1 * Y, Nm1 * Z)
            End Get
        End Property

        Public Sub Normalize()
            Dim Nm1 As Double = 1 / Math.Sqrt(X * X + Y * Y + Z * Z)
            Scale(Nm1)
        End Sub

#End Region

#Region " Linear operations "

        ''' <summary>
        ''' Multiplies the vector by a given scalar factor.
        ''' </summary>
        ''' <param name="Factor"></param>
        ''' <remarks></remarks>
        Public Sub Scale(ByVal Factor As Double)
            X = X * Factor
            Y = Y * Factor
            Z = Z * Factor
        End Sub

        ''' <summary>
        ''' Rotatets the vector in euler angles.
        ''' </summary>
        ''' <param name="Psi"></param>
        ''' <param name="Tita"></param>
        ''' <param name="Fi"></param>
        ''' <remarks></remarks>
        Public Sub Rotate(ByVal Psi As Double, ByVal Tita As Double, ByVal Fi As Double)

            Dim RotM As New RotationMatrix
            Dim Px As Double
            Dim Py As Double
            Dim Pz As Double

            Px = X
            Py = Y
            Pz = Z

            RotM.Generate(Psi * Math.PI / 180, Tita * Math.PI / 180, Fi * Math.PI / 180)

            Me.X = Px * RotM.Item(1, 1) + Py * RotM.Item(1, 2) + Pz * RotM.Item(1, 3)
            Me.Y = Px * RotM.Item(2, 1) + Py * RotM.Item(2, 2) + Pz * RotM.Item(2, 3)
            Me.Z = Px * RotM.Item(3, 1) + Py * RotM.Item(3, 2) + Pz * RotM.Item(3, 3)

        End Sub

        ''' <summary>
        ''' Rotatets the vector with a given rotation matrix.
        ''' </summary>
        ''' <param name="RotM"></param>
        ''' <remarks></remarks>
        Public Sub Rotate(ByVal RotM As RotationMatrix)

            Dim Px As Double
            Dim Py As Double
            Dim Pz As Double

            Px = X
            Py = Y
            Pz = Z

            X = Px * RotM.Item(1, 1) + Py * RotM.Item(1, 2) + Pz * RotM.Item(1, 3)
            Y = Px * RotM.Item(2, 1) + Py * RotM.Item(2, 2) + Pz * RotM.Item(2, 3)
            Z = Px * RotM.Item(3, 1) + Py * RotM.Item(3, 2) + Pz * RotM.Item(3, 3)

        End Sub

        Public Sub Rotate(ByVal ReferencePoint As Vector3, ByVal RotM As RotationMatrix)
            Substract(ReferencePoint)
            Rotate(RotM)
            Add(ReferencePoint)
        End Sub

        ''' <summary>
        ''' Projects the vector on a given direction.
        ''' </summary>
        ''' <param name="Direction"></param>
        ''' <remarks></remarks>
        Public Sub ProjectOnVector(ByVal Direction As Vector3)
            Dim Proyección As Double = InnerProduct(Direction)
            Me.Assign(Direction, Proyección)
        End Sub

        ''' <summary>
        ''' Projects the vector on a given plane.
        ''' </summary>
        ''' <param name="NormalDirection"></param>
        ''' <remarks></remarks>
        Public Sub ProjectOnPlane(ByVal NormalDirection As Vector3)
            Dim Proyección As New Vector3
            Dim dp As Double = Me.X * NormalDirection.X + Me.Y * NormalDirection.Y + Me.Z * NormalDirection.Z
            Proyección.X = dp * NormalDirection.X
            Proyección.Y = dp * NormalDirection.Y
            Proyección.Z = dp * NormalDirection.Z
            Me.X -= Proyección.X
            Me.Y -= Proyección.Y
            Me.Z -= Proyección.Z
        End Sub

        ''' <summary>
        ''' Returns the projection of the vector on a given direction.
        ''' </summary>
        ''' <param name="Direction"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function GetProjection(ByVal Direction As Vector3) As Vector3
            Dim NuevoVector As New Vector3
            Direction.Normalize()
            NuevoVector.X = Direction.X * InnerProduct(Direction)
            NuevoVector.Y = Direction.Y * InnerProduct(Direction)
            NuevoVector.Y = Direction.Y * InnerProduct(Direction)
            Return NuevoVector

        End Function

        Public Sub Transform(M As Matrix3x3)

            Dim _X As Double = X
            Dim _Y As Double = Y
            Dim _Z As Double = Z


            X = _X * M.Item(1, 1) + _Y * M.Item(1, 2) + _Z * M.Item(1, 3)
            Y = _X * M.Item(2, 1) + _Y * M.Item(2, 2) + _Z * M.Item(2, 3)
            Z = _X * M.Item(3, 1) + _Y * M.Item(3, 2) + _Z * M.Item(3, 3)

        End Sub

        Public Sub Transform(Base As Base3)

            Dim _X As Double = X
            Dim _Y As Double = Y
            Dim _Z As Double = Z

            X = _X * Base.U.X + _Y * Base.U.Y + _Z * Base.U.Z
            Y = _X * Base.V.X + _Y * Base.V.Y + _Z * Base.V.Z
            Z = _X * Base.W.X + _Y * Base.W.Y + _Z * Base.W.Z

        End Sub

        Public Sub AntiTransform(Base As Base3)

            Dim _X As Double = X
            Dim _Y As Double = Y
            Dim _Z As Double = Z

            X = _X * Base.U.X + _Y * Base.V.X + _Z * Base.W.X
            Y = _X * Base.U.Y + _Y * Base.V.Y + _Z * Base.W.Y
            Z = _X * Base.U.Z + _Y * Base.V.Z + _Z * Base.W.Z

        End Sub

#End Region

#Region " Operaciones vectoriales "

        Public Function VectorProduct(ByVal Vector As Vector3) As Vector3
            Dim Producto As New Vector3
            Producto.X = Y * Vector.Z - Z * Vector.Y
            Producto.Y = Z * Vector.X - X * Vector.Z
            Producto.Z = X * Vector.Y - Y * Vector.X
            Return Producto
        End Function

        Public Sub VectorMultiplication(ByVal Vector As Vector3)
            Dim Producto As New Vector3(Me)
            Dim Xo As Double = X
            Dim Yo As Double = Y
            Dim Zo As Double = Z
            X = Yo * Vector.Z - Zo * Vector.Y
            Y = Zo * Vector.X - Xo * Vector.Z
            Z = Xo * Vector.Y - Yo * Vector.X
        End Sub

        Public Sub FromVectorProduct(ByVal V1 As Vector3, ByVal V2 As Vector3)
            X = V1.Y * V2.Z - V1.Z * V2.Y
            Y = V1.Z * V2.X - V1.X * V2.Z
            Z = V1.X * V2.Y - V1.Y * V2.X
        End Sub

        Public Sub FromSubstraction(ByVal V1 As Vector3, ByVal V2 As Vector3)
            X = V1.X - V2.X
            Y = V1.Y - V2.Y
            Z = V1.Z - V2.Z
        End Sub

        Public Function GetVectorToPoint(ByVal Punto As Vector3) As Vector3
            Dim Diferencia As New Vector3
            Diferencia.X = Punto.X - Me.X
            Diferencia.Y = Punto.Y - Me.Y
            Diferencia.Z = Punto.Z - Me.Z
            Return Diferencia
        End Function

#End Region

#Region " Other operations "

        Public Sub SetToCero()
            X = 0
            Y = 0
            Z = 0
        End Sub

        Public Overloads Sub Assign(ByVal Vector As Vector3)
            X = Vector.X
            Y = Vector.Y
            Z = Vector.Z
        End Sub

        Public Overloads Sub Assign(ByVal Vector As Vector3, ByVal Factor As Double)
            X = Factor * Vector.X
            Y = Factor * Vector.Y
            Z = Factor * Vector.Z
        End Sub

        Public Sub ReadFromString(ByVal Line As String, Optional ByVal Margen As Integer = 1)
            X = CDbl(Mid(Line, Margen, 25))
            Y = CDbl(Mid(Line, Margen + 25, 25))
            Z = CDbl(Right(Line, 25))
        End Sub

        Public Function Clone() As Vector3
            Return New Vector3(Me)
        End Function

#End Region

    End Class

    ''' <summary>
    ''' Represents a basis of orthonormal vectors on the euclidean space.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Base3

        Public U As New Vector3
        Public V As New Vector3
        Public W As New Vector3

        ''' <summary>
        ''' Sets the standard vectors.
        ''' </summary>
        Public Sub CanonicalBase()

            U.X = 1.0
            U.Y = 0.0
            U.Z = 0.0

            V.X = 0.0
            V.Y = 1.0
            V.Z = 0.0

            W.X = 0.0
            W.Y = 0.0
            W.Z = 1.0

        End Sub

    End Class

    ''' <summary>
    ''' Represents a line in space.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Line3

        Public Property Point As New Vector3
        Public Property Direction As New Vector3

    End Class

End Namespace

Public Class LimitValues

    Public Property Maximum As Double
    Public Property Minimum As Double

End Class
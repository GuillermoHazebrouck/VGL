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

'' Standard .NET dependencies
'-----------------------------------------------------------------------------
Imports System.IO

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.MathTools.Algebra.EuclideanSpace

'##################################################################################################
' Unit: AirLoads
'
' This unit provides a declaration of air loads.
'##################################################################################################
Namespace AeroTools.Models.Aero

    '//////////////////////////////////////////////////////////////////////////////////////////////
    ' Gathers the resultants of aerodynamic forces per type. 
    ' All forces are normalized by normalized by qS (using the properties Area and DynamicPressure).
    '//////////////////////////////////////////////////////////////////////////////////////////////
    Public Class AirLoads

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The reference area (the default value is 1.0).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Area As Double = 1.0

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The reference length (the default value is 1.0).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Length As Double = 1.0

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The reference dynamic pressure [Pa].
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property DynamicPressure As Double = 0.0

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The total force.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Force As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The total moment about the origin.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Moment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The lift force from the slender panels [N]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property LiftForce As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The lift moment about the origin from the slender panels [Nm]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property LiftMoment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The induced drag force from the slender panels [N]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property InducedDragForce As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The induced drag moment about the origin from the slender panels [Nm]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property InducedDragMoment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The skin drag force from the slender panels extracted from the polar 
        ' curves using the lift coefficient and the local Reynolds number [N].
        ' There is an option to include here the body skin drag using a very 
        ' simplistic model (under development).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SkinDragForce As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The skin drag moment about the origin from the slender panels [Nm]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SkinDragMoment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The force from the closed surface panels [Nm]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property BodyForce As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The body moment about the origin from the closed surface panels [Nm]
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property BodyMoment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The lift coefficient.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property LiftCoefficient As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The induced drag coefficient.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property InducedDragCoefficient As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The skin drag coefficient.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SkinDragCoefficient As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The normal incidence angle
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Alfa As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The lateral incidence angle
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Beta As Double



        '==========================================================================================
        ' Adds the given load scaled by the surface.
        '==========================================================================================
        Public Sub Add(Airloads As AirLoads)

            Force.Add(Airloads.Force)
            Moment.Add(Airloads.Moment)

            LiftForce.Add(Airloads.LiftForce)
            LiftMoment.Add(Airloads.LiftMoment)

            InducedDragForce.Add(Airloads.InducedDragForce)
            InducedDragMoment.Add(Airloads.InducedDragMoment)

            SkinDragForce.Add(Airloads.SkinDragForce)
            SkinDragMoment.Add(Airloads.SkinDragMoment)

            BodyForce.Add(Airloads.BodyForce)
            BodyMoment.Add(Airloads.BodyMoment)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Clears the airloads to zero.
        '==========================================================================================
        Public Sub Clear()

            Force.SetToCero()
            Moment.SetToCero()

            LiftCoefficient = 0.0
            InducedDragCoefficient = 0.0
            SkinDragCoefficient = 0.0

            LiftForce.SetToCero()
            LiftMoment.SetToCero()

            InducedDragForce.SetToCero()
            InducedDragMoment.SetToCero()

            SkinDragForce.SetToCero()
            SkinDragMoment.SetToCero()

            BodyForce.SetToCero()
            BodyMoment.SetToCero()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Applies the transformation to all vectors
        '==========================================================================================
        Public Sub Transform(Base As Base3)

            Force.Transform(Base)
            Moment.Transform(Base)

            LiftForce.Transform(Base)
            LiftMoment.Transform(Base)

            InducedDragForce.Transform(Base)
            InducedDragMoment.Transform(Base)

            SkinDragForce.Transform(Base)
            SkinDragMoment.Transform(Base)

            BodyForce.Transform(Base)
            BodyMoment.Transform(Base)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Applies the anti transformation to all vectors
        '==========================================================================================
        Public Sub AntiTransform(Base As Base3)

            Force.AntiTransform(Base)
            Moment.AntiTransform(Base)

            LiftForce.AntiTransform(Base)
            LiftMoment.AntiTransform(Base)

            InducedDragForce.AntiTransform(Base)
            InducedDragMoment.AntiTransform(Base)

            SkinDragForce.AntiTransform(Base)
            SkinDragMoment.AntiTransform(Base)

            BodyForce.AntiTransform(Base)
            BodyMoment.AntiTransform(Base)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Writes the airloads data to a binary stream
        '==========================================================================================
        Public Sub WriteBinary(ByRef Stream As BinaryWriter)

            Stream.Write(Force.X)
            Stream.Write(Force.Y)
            Stream.Write(Force.Z)

            Stream.Write(Moment.X)
            Stream.Write(Moment.Y)
            Stream.Write(Moment.Z)

            Stream.Write(LiftCoefficient)
            Stream.Write(InducedDragCoefficient)
            Stream.Write(SkinDragCoefficient)

            Stream.Write(LiftForce.X)
            Stream.Write(LiftForce.Y)
            Stream.Write(LiftForce.Z)

            Stream.Write(LiftMoment.X)
            Stream.Write(LiftMoment.Y)
            Stream.Write(LiftMoment.Z)

            Stream.Write(InducedDragForce.X)
            Stream.Write(InducedDragForce.Y)
            Stream.Write(InducedDragForce.Z)

            Stream.Write(InducedDragMoment.X)
            Stream.Write(InducedDragMoment.Y)
            Stream.Write(InducedDragMoment.Z)

            Stream.Write(SkinDragForce.X)
            Stream.Write(SkinDragForce.Y)
            Stream.Write(SkinDragForce.Z)

            Stream.Write(SkinDragMoment.X)
            Stream.Write(SkinDragMoment.Y)
            Stream.Write(SkinDragMoment.Z)

            Stream.Write(BodyForce.X)
            Stream.Write(BodyForce.Y)
            Stream.Write(BodyForce.Z)

            Stream.Write(BodyMoment.X)
            Stream.Write(BodyMoment.Y)
            Stream.Write(BodyMoment.Z)

            Stream.Write(Area)
            Stream.Write(Length)
            Stream.Write(DynamicPressure)
            Stream.Write(Alfa)
            Stream.Write(Beta)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the airloads data from a binary stream.
        '==========================================================================================
        Public Sub ReadBinary(ByRef Stream As System.IO.BinaryReader)

            Force.X = Stream.ReadDouble
            Force.Y = Stream.ReadDouble
            Force.Z = Stream.ReadDouble

            Moment.X = Stream.ReadDouble
            Moment.Y = Stream.ReadDouble
            Moment.Z = Stream.ReadDouble

            LiftCoefficient = Stream.ReadDouble
            InducedDragCoefficient = Stream.ReadDouble
            SkinDragCoefficient = Stream.ReadDouble

            LiftForce.X = Stream.ReadDouble
            LiftForce.Y = Stream.ReadDouble
            LiftForce.Z = Stream.ReadDouble

            LiftMoment.X = Stream.ReadDouble
            LiftMoment.Y = Stream.ReadDouble
            LiftMoment.Z = Stream.ReadDouble

            InducedDragForce.X = Stream.ReadDouble
            InducedDragForce.Y = Stream.ReadDouble
            InducedDragForce.Z = Stream.ReadDouble

            InducedDragMoment.X = Stream.ReadDouble
            InducedDragMoment.Y = Stream.ReadDouble
            InducedDragMoment.Z = Stream.ReadDouble

            SkinDragForce.X = Stream.ReadDouble
            SkinDragForce.Y = Stream.ReadDouble
            SkinDragForce.Z = Stream.ReadDouble

            SkinDragMoment.X = Stream.ReadDouble
            SkinDragMoment.Y = Stream.ReadDouble
            SkinDragMoment.Z = Stream.ReadDouble

            BodyForce.X = Stream.ReadDouble
            BodyForce.Y = Stream.ReadDouble
            BodyForce.Z = Stream.ReadDouble

            BodyMoment.X = Stream.ReadDouble
            BodyMoment.Y = Stream.ReadDouble
            BodyMoment.Z = Stream.ReadDouble

            Area = Stream.ReadDouble
            Length = Stream.ReadDouble
            DynamicPressure = Stream.ReadDouble
            Alfa = Stream.ReadDouble
            Beta = Stream.ReadDouble

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
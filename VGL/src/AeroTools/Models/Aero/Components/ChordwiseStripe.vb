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

'#############################################################################
' Unit: ChordWiseStripe
'
' This unit provides a class that represents a collection of chordwise panels
' in a slender surface. Each stripe contains a polar curve that is used to
' predict the local skin drag based on the local lift.
'#############################################################################
Namespace AeroTools.Models.Aero.Components

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a collection of adjacent vortex rings (typically along the chord of a wing) 
    ' where lift and drag can be locally computed.
    ' Note: the stripe force coefficients are referred to the local velocity, not to the 
    ' reference stream velocity (with the idea of quantifying the situation on the airfoil).
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class ChorwiseStripe

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Chordwise stripe of vortex rings (from the leading edge to the trailing edge).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Rings As List(Of VortexRing)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Polar curve used to compute the local drag.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Polars As PolarFamily

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Initializes the object.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New()

            Rings = New List(Of VortexRing)

        End Sub
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Returns the area of this portion of wing in m².
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Area As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local lift coefficient in this portion of wing.
        ' Note: the reference velocity is the local velocity.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property LiftCoefficient As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local induced drag coefficient in this portion of wing.
        ' Note: the reference velocity is the local velocity.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property InducedDragCoefficient As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Skin drag coefficient in this portion of wing.
        ' Note: the reference velocity is the local velocity.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property SkinDragCoefficient As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Total stripe lift in N.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Lift As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Total stripe induced drag in N.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property InducedDrag As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Total stripe induced drag in N.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property SkinDrag As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Total stripe moment (with respect to the origin) in N.m.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property LiftMoment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Total stripe moment (with respect to the origin) in N.m.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property InducedDragMoment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Total stripe moment (with respect to the origin) in N.m.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property SkinDragMoment As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stripe chord in meters.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property Chord As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Vector having the direction of the chord.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property ChordWiseVector As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Point located at the geometric center of the chordwise stripe.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property CenterPoint As New Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Calculates the stripe lift, drag and area, along with the correspondig moment and 
        ' reference coefficients. 
        ' The Cp (pressure coefficient) and the Cdi (local induced component) of each panel 
        ' should be calculated before calling this sub.
        ' NOTE:
        ' This method corrects the missing leading edge pressure decay by substracting the 
        ' projection of the total force in the direction of the local stream velocity (this is 
        ' why the stream direction is requested). This is an extention of the 2D potential theory
        ' in which 2D airfoils do not introduce drag. This is arguable, but at least the  
        ' correction does provide more consistent results, specially in the case of rotating 
        ' wings where the incidence varies considerably.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ComputeLoads(StreamVelocity As Vector3,
                                StreamRotation As Vector3,
                                Density As Double,
                                Viscosity As Double)

            ' Calculate local chordwise direction and chord:

            Dim n As Integer = Rings.Count - 1

            _ChordWiseVector.X = 0.5 * ((Rings(n).Node(2).Position.X + Rings(n).Node(3).Position.X) - (Rings(0).Node(1).Position.X + Rings(0).Node(4).Position.X))
            _ChordWiseVector.Y = 0.5 * ((Rings(n).Node(2).Position.Y + Rings(n).Node(3).Position.Y) - (Rings(0).Node(1).Position.Y + Rings(0).Node(4).Position.Y))
            _ChordWiseVector.Z = 0.5 * ((Rings(n).Node(2).Position.Z + Rings(n).Node(3).Position.Z) - (Rings(0).Node(1).Position.Z + Rings(0).Node(4).Position.Z))

            _CenterPoint.X = 0.5 * (Rings(0).Node(1).Position.X + Rings(0).Node(4).Position.X + _ChordWiseVector.X)
            _CenterPoint.Y = 0.5 * (Rings(0).Node(1).Position.Y + Rings(0).Node(4).Position.Y + _ChordWiseVector.Y)
            _CenterPoint.Z = 0.5 * (Rings(0).Node(1).Position.Z + Rings(0).Node(4).Position.Z + _ChordWiseVector.Z)

            _Chord = _ChordWiseVector.Norm2
            _ChordWiseVector.Normalize()

            Dim DynamicPressure As Double = 0.5 * StreamVelocity.SquareEuclideanNorm * Density
            Dim LocalVelocity As New Vector3(StreamVelocity)
            LocalVelocity.AddCrossProduct(StreamRotation, _CenterPoint)
            Dim StreamDirection As New Vector3(LocalVelocity)
            StreamDirection.Normalize()
    
            _Area = 0.0#
            _LiftCoefficient = 0.0#
            _SkinDragCoefficient = 0.0#
            _InducedDragCoefficient = 0.0#

            Dim Projection As Double
            Dim Force As Double

            Dim LocalLift = New Vector3
            Dim LocalInducedDrag = New Vector3
            Dim qS As Double = 0.0#

            _Lift.SetToCero()
            _InducedDrag.SetToCero()
            _SkinDrag.SetToCero()
            _LiftMoment.SetToCero()
            _InducedDragMoment.SetToCero()
            _SkinDragMoment.SetToCero()

            ' Sum the lift and induced drag contributions from each panel
            '------------------------------------------------------------

            For Each VortexRing In Rings

                _Area += VortexRing.Area

                qS = VortexRing.Area * DynamicPressure

                ' Lift
                '-------------------------------------------------------

                Force = VortexRing.Cp * qS
                Projection = VortexRing.Normal.InnerProduct(StreamDirection)

                LocalLift.X = Force * (VortexRing.Normal.X - Projection * StreamDirection.X)
                LocalLift.Y = Force * (VortexRing.Normal.Y - Projection * StreamDirection.Y)
                LocalLift.Z = Force * (VortexRing.Normal.Z - Projection * StreamDirection.Z)

                _Lift.X += LocalLift.X
                _Lift.Y += LocalLift.Y
                _Lift.Z += LocalLift.Z

                _LiftMoment.AddCrossProduct(VortexRing.ControlPoint, LocalLift)

                ' Induced drag 
                '-------------------------------------------------------

                Dim qS_Cdi As Double = qS * VortexRing.Cdi
                LocalInducedDrag.X = qS_Cdi * StreamDirection.X
                LocalInducedDrag.Y = qS_Cdi * StreamDirection.Y
                LocalInducedDrag.Z = qS_Cdi * StreamDirection.Z

                _InducedDrag.X += LocalInducedDrag.X
                _InducedDrag.Y += LocalInducedDrag.Y
                _InducedDrag.Z += LocalInducedDrag.Z

                _InducedDragMoment.AddCrossProduct(VortexRing.ControlPoint, LocalInducedDrag)

            Next

            ' Calculate the force coefficients
            ' Note: all coefficients are based on the local dynamic pressure
            '--------------------------------------------------------------------------

            qS = _Area * 0.5 * LocalVelocity.SquareEuclideanNorm * Density

            _LiftCoefficient = _Lift.Norm2 / qS

            _InducedDragCoefficient = InducedDrag.Norm2 / qS

            ' Skin drag from polar curve using the local lift coefficient and Reynolds. 
            ' Note: if there is no polar curve its value stays zero.
            '--------------------------------------------------------------------------

            If Polars IsNot Nothing Then

                Dim Reynolds As Double = LocalVelocity.Norm2 * Density * Chord / Viscosity

                _SkinDragCoefficient = Polars.SkinDrag(LiftCoefficient, Reynolds)

            End If

            _SkinDrag.X = _SkinDragCoefficient * ChordWiseVector.X * qS
            _SkinDrag.Y = _SkinDragCoefficient * ChordWiseVector.Y * qS
            _SkinDrag.Z = _SkinDragCoefficient * ChordWiseVector.Z * qS

            _SkinDragMoment.FromVectorProduct(CenterPoint, SkinDrag)

        End Sub
        '------------------------------------------------------------------------------------------



        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads the chordwise link data from a binary stream.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Sub ReadBinary(ByRef Stream As BinaryReader, ByRef InRings As List(Of VortexRing), ByRef PolarData As PolarDatabase)
            Try
                ' Rings
                '-----------------------------------
                For I = 1 To Stream.ReadInt32
                    Rings.Add(InRings(Stream.ReadInt32))
                Next

                ' Polar id
                '-----------------------------------
                Dim PolarId = New Guid(Stream.ReadString())
                Polars = PolarData.GetFamilyFromId(PolarId)

                ' Geometric properties
                '-----------------------------------

                _Chord = Stream.ReadDouble
                _Area = Stream.ReadDouble

                _ChordWiseVector.X = Stream.ReadDouble
                _ChordWiseVector.Y = Stream.ReadDouble
                _ChordWiseVector.Z = Stream.ReadDouble

                _CenterPoint.X = Stream.ReadDouble
                _CenterPoint.Y = Stream.ReadDouble
                _CenterPoint.Z = Stream.ReadDouble

                ' Forces and moments
                '-----------------------------------

                _Lift.X = Stream.ReadDouble
                _Lift.Y = Stream.ReadDouble
                _Lift.Z = Stream.ReadDouble
                _LiftCoefficient = Stream.ReadDouble

                _InducedDrag.X = Stream.ReadDouble
                _InducedDrag.Y = Stream.ReadDouble
                _InducedDrag.Z = Stream.ReadDouble
                _InducedDragCoefficient = Stream.ReadDouble

                _SkinDrag.X = Stream.ReadDouble
                _SkinDrag.Y = Stream.ReadDouble
                _SkinDrag.Z = Stream.ReadDouble
                _SkinDragCoefficient = Stream.ReadDouble

                _LiftMoment.X = Stream.ReadDouble
                _LiftMoment.Y = Stream.ReadDouble
                _LiftMoment.Z = Stream.ReadDouble

                _InducedDragMoment.X = Stream.ReadDouble
                _InducedDragMoment.Y = Stream.ReadDouble
                _InducedDragMoment.Z = Stream.ReadDouble

                _SkinDragMoment.X = Stream.ReadDouble
                _SkinDragMoment.Y = Stream.ReadDouble
                _SkinDragMoment.Z = Stream.ReadDouble

            Catch ex As Exception
                Rings.Clear()
            End Try

        End Sub
        '------------------------------------------------------------------------------------------



        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes the chordwise link data to a binary stream.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Sub WriteBinary(ByRef Stream As BinaryWriter)

            ' Rings
            '-----------------------------------
            Stream.Write(Rings.Count)

            For Each Ring In Rings
                Stream.Write(Ring.IndexL)
            Next

            ' Polar id
            '-----------------------------------
            If Polars Is Nothing Then
                Stream.Write(Guid.Empty.ToString)
            Else
                Stream.Write(Polars.Id.ToString)
            End If

            ' Geometric properties
            '-----------------------------------

            Stream.Write(Chord)
            Stream.Write(Area)

            Stream.Write(ChordWiseVector.X)
            Stream.Write(ChordWiseVector.Y)
            Stream.Write(ChordWiseVector.Z)

            Stream.Write(CenterPoint.X)
            Stream.Write(CenterPoint.Y)
            Stream.Write(CenterPoint.Z)

            ' Forces and moments
            '-----------------------------------

            Stream.Write(Lift.X)
            Stream.Write(Lift.Y)
            Stream.Write(Lift.Z)
            Stream.Write(LiftCoefficient)

            Stream.Write(InducedDrag.X)
            Stream.Write(InducedDrag.Y)
            Stream.Write(InducedDrag.Z)
            Stream.Write(InducedDragCoefficient)

            Stream.Write(SkinDrag.X)
            Stream.Write(SkinDrag.Y)
            Stream.Write(SkinDrag.Z)
            Stream.Write(SkinDragCoefficient)

            Stream.Write(LiftMoment.X)
            Stream.Write(LiftMoment.Y)
            Stream.Write(LiftMoment.Z)

            Stream.Write(InducedDragMoment.X)
            Stream.Write(InducedDragMoment.Y)
            Stream.Write(InducedDragMoment.Z)

            Stream.Write(SkinDragMoment.X)
            Stream.Write(SkinDragMoment.Y)
            Stream.Write(SkinDragMoment.Z)

        End Sub
        '------------------------------------------------------------------------------------------



    End Class

End Namespace
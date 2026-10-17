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
Imports System.Xml

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports VGL.AeroTools.IoHelper

'#############################################################################
' Unit: AeroelasticHistogram
'
' This unit contains the definition of an histogram to be used on aeroelastic
' simulations. It gives the stream velocity and modal damping as a function
' of time.
'#############################################################################
Namespace AeroTools.Settings

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The data on a particular time step
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class StepData

        Public Property Time As Double
        Public Property Damping As Double
        Public Property Velocity As New Vector3

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Contains the velocity and damping functions in the time domain.
    ' The aeroelastic histogram is used to simulate a stream perturbation (gust)
    ' starting from a steady state condition.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class AeroelasticHistogram

        Private _HyperDamping As Double
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The hyper damping is the value of damping that will be imposed at the
        ' beginning of the simulation so that convergence is reached before the
        ' gust is applied.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property HyperDamping As Double
            Set(value As Double)
                _HyperDamping = value
                RaiseEvent ValueChanged()
            End Set
            Get
                Return _HyperDamping
            End Get
        End Property
        '------------------------------------------------------------------------------------------
        
        Private _HyperDampingSpan As Double
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The duration of the hyper damping, from the beginning of the simulation.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property HyperDampingSpan As Double
            Set(value As Double)
                _HyperDampingSpan = value
                RaiseEvent ValueChanged()
            End Set
            Get
                Return _HyperDampingSpan
            End Get
        End Property
        '------------------------------------------------------------------------------------------

        Private _NormalDamping As Double
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The damping that will be applied after the hyper damping and before the gust.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property NormalDamping As Double
            Set(value As Double)
                _NormalDamping = value
                RaiseEvent ValueChanged()
            End Set
            Get
                Return _NormalDamping
            End Get
        End Property
        '------------------------------------------------------------------------------------------

        Private _GustX As Double
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The intensity of the stream perturbation in the X direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property GustX As Double
            Set(value As Double)
                _GustX = value
                RaiseEvent ValueChanged()
            End Set
            Get
                Return _GustX
            End Get
        End Property
        '------------------------------------------------------------------------------------------

        Private _GustY As Double
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The intensity of the stream perturbation in the Y direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property GustY As Double
            Set(value As Double)
                _GustY = value
                RaiseEvent ValueChanged()
            End Set
            Get
                Return _GustY
            End Get
        End Property
        '------------------------------------------------------------------------------------------

        Private _GustZ As Double
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The intensity of the stream perturbation in the Z direction
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property GustZ As Double
            Set(value As Double)
                _GustZ = value
                RaiseEvent ValueChanged()
            End Set
            Get
                Return _GustZ
            End Get
        End Property
        '------------------------------------------------------------------------------------------

        Private _GustSpan As Double
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The duration of the gust (after the hyper damping period).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property GustSpan As Double
            Set(value As Double)
                _GustSpan = value
                RaiseEvent ValueChanged()
            End Set
            Get
                Return _GustSpan
            End Get
        End Property
        '------------------------------------------------------------------------------------------
        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private _Steps As New List(Of StepData)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        'Event triggerd when a value changes. This can be used in a graphical interface.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Event ValueChanged()

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Returns the gust and damping at the given time step.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property State(TimeStep As Integer) As StepData
            Get
                Return _Steps(TimeStep)
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Generates the gust and daming functions for the given number of steps and
        ' interval between steps.
        ' Velocity is the reference stream velocity
        ' TimeStep is the interval between two steps (must be constant on the simulation)
        ' Steps is the total number of time steps
        '==========================================================================================
        Public Sub Generate(Velocity As Vector3, TimeStep As Double, Steps As Integer)

            _Steps.Clear()

            Dim i As Integer = 0

            Dim FirstStep As New StepData
            FirstStep.Damping = HyperDamping
            FirstStep.Velocity.Assign(Velocity)
            FirstStep.Time = i * TimeStep
            _Steps.Add(FirstStep)

            While _Steps(i).Time < HyperDampingSpan And i < Steps

                Dim NewStep As New StepData

                NewStep.Damping = HyperDamping
                NewStep.Velocity.Assign(Velocity)
                NewStep.Time = i * TimeStep
                i += 1

                _Steps.Add(NewStep)

            End While

            Dim t As Double = 0.0

            While _Steps(i).Time < HyperDampingSpan + GustSpan And i < Steps

                Dim NewStep As New StepData

                NewStep.Damping = NormalDamping
                NewStep.Velocity.Assign(Velocity)

                Dim f As Double = 0.5 * (Math.Sin(2 * Math.PI * (t / GustSpan - 0.25)) + 1)

                NewStep.Velocity.X += f * GustX
                NewStep.Velocity.Y += f * GustY
                NewStep.Velocity.Z += f * GustZ

                t += TimeStep

                NewStep.Time = _Steps(_Steps.Count - 1).Time + TimeStep

                i += 1

                _Steps.Add(NewStep)

            End While

            While i < Steps

                Dim NewStep As New StepData
                NewStep.Damping = NormalDamping
                NewStep.Velocity.Assign(Velocity)
                NewStep.Time = i * TimeStep
                i += 1

                _Steps.Add(NewStep)

            End While

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Deep copy of the object
        '==========================================================================================
        Public Function Clone()

            Dim ClonedHistogram As AeroelasticHistogram = New AeroelasticHistogram
            ClonedHistogram.GustX = GustX
            ClonedHistogram.GustY = GustY
            ClonedHistogram.GustZ = GustZ
            ClonedHistogram.GustSpan = GustSpan
            ClonedHistogram.HyperDamping = HyperDamping
            ClonedHistogram.HyperDampingSpan = HyperDampingSpan
            ClonedHistogram.NormalDamping = NormalDamping

            Return ClonedHistogram

        End Function
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        '
        '==========================================================================================
        Public Sub New()

            HyperDamping = 6
            HyperDampingSpan = 0.55
            NormalDamping = 0.01
            GustX = 0
            GustY = 0
            GustZ = 0
            GustSpan = 0.4

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Writes the histogram parameters in an XML node
        '==========================================================================================
        Sub SaveToXML(ByRef Stream As XmlWriter)

            Stream.WriteStartElement("FlutterTestHistogram")
            Stream.WriteAttributeString("HyperDamping", String.Format("{0}", HyperDamping))
            Stream.WriteAttributeString("NormalDamping", String.Format("{0}", NormalDamping))
            Stream.WriteAttributeString("HyperDampingSpan", String.Format("{0}", HyperDampingSpan))
            Stream.WriteAttributeString("GustIntensityX", String.Format("{0}", GustX))
            Stream.WriteAttributeString("GustIntensityY", String.Format("{0}", GustY))
            Stream.WriteAttributeString("GustIntensityZ", String.Format("{0}", GustZ))
            Stream.WriteAttributeString("GustSpan", String.Format("{0}", GustSpan))
            Stream.WriteEndElement()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the histogram parameters from an XML node
        '==========================================================================================
        Public Sub ReadFromXML(ByRef Stream As XmlReader)

            While Stream.Read

                If Stream.NodeType = XmlNodeType.Element Then

                    Select Case Stream.Name

                        Case "FlutterTestHistogram"

                            HyperDamping = IOXML.ReadDouble(Stream, "HyperDamping", 1.0)
                            NormalDamping = IOXML.ReadDouble(Stream, "NormalDamping", 0.01)
                            HyperDampingSpan = IOXML.ReadDouble(Stream, "HyperDampingSpan", 1.0)
                            GustX = IOXML.ReadDouble(Stream, "GustIntensityX", 0.0)
                            GustY = IOXML.ReadDouble(Stream, "GustIntensityY", 0.0)
                            GustZ = IOXML.ReadDouble(Stream, "GustIntensityZ", 0.0)
                            GustSpan = IOXML.ReadDouble(Stream, "GustSpan", 0.0)

                    End Select

                End If

            End While

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
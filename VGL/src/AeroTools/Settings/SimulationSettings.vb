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
' Unit: SimulationSettings
'
' This unit provides the class that gathers all simulation settings necessary
' for the calculations.
'#############################################################################
Namespace AeroTools.Settings

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' A sence bit.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum Sence As Integer
        Positive = 1
        Negative = -1
    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The different options to solve the linear equations.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum MatrixSolverType As Byte
        LU = 0
        QR = 1
    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Enumerates the adjacent rings on a given ring.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum AdjacentRing As Byte
        Panel1 = 0
        Panel2 = 1
        Panel3 = 2
        Panel4 = 3
    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The possible kind of simulation.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum CalculationType As Byte
        SteadyState = 0
        FreeFlight = 1
        Aeroelastic = 2
    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Gathers all settings necessary for the calculation.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class SimulationSettings

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Free stream velocity vector in m/s.
        ' The velocity components are scaled by an amplitude factor in unsteady problems.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property StreamVelocity As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Angular velocity of the aircraft reference frame in rad/s.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property StreamRotation As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Free stream density in kg/m³.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Density As Double = 1.225#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Free stream static pressure in Pa.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property StaticPressure As Double = 101300.0#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Free stream viscosity in kg/(m.s).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Viscocity As Double = 0.0000178#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Specifies the number of integration steps.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SimulationSteps As Integer = 1

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The gravity intensity (assumed in the global Z direction)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Gravity As Double = -9.8

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The model mass in kg
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Mass As Double = 0.0#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The model center of gravity
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CenterOfGravity As New Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The model second moment of inertia about the X axis in kg.m²
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Ixx As Double = 0.0#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The model second moment of inertia about the Y axis  in kg.m²
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Iyy As Double = 0.0#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The model second moment of inerti about the Z axis a in kg.m²
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Izz As Double = 0.0#

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The main directions of inertia
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property InertialBasis As New Base3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The number of rigid steps in a free flight simulation
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FreeFlightStartStep As Integer = 50

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The number of correction steps in a free flight simulation
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CorrectionSteps As Integer = 10

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Specifies the size of the instegration step in seconds.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Interval As Double = 0.1

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Specifies the radius of the region around vortices where the velocity is null in meters.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Cutoff As Double = 0.0001

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Specifies whether the cutoff has to be automatically estimated based on the mesh.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CalculateCutoff As Boolean

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Contains the necessary structural settings.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property StructuralSettings As New Models.Structural.Library.StructuralSettings

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Amplitude of the free stream velocity components for every time step in m/s.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property UnsteadyVelocity As New UnsteadyVelocity

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Contains information about how the simulation parameters vary during an aeroelastic analysis.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property AeroelasticHistogram As New AeroelasticHistogram

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The type of analysis to be performed.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property AnalysisType As CalculationType = CalculationType.SteadyState

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Maximum distance between two rings to be considered as adjacent, in meters.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SurveyTolerance As Double = 0.001

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the influence of the wakes on the fuselage should be included.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property StrongWakeInfluence As Boolean = False

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the GPU should be used for some of the computations.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property UseGpu As Boolean = False

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The id of the Gpu device to use (in case the UseGpu is true).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property GpuDeviceId As Integer = 0

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the wakes must be extended in the stream direction after the trimming step.
        ' NOTE: The current extension is 100m, and cannot be adapted.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property ExtendWakes As Boolean = False



        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Sets the default values.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub InitializeParameters()

            StreamVelocity = New Vector3
            StreamVelocity.X = 10.0
            StreamVelocity.Y = 0
            StreamVelocity.Z = 0

            StreamRotation = New Vector3
            StreamRotation.X = 0
            StreamRotation.Y = 0
            StreamRotation.Z = 0

            Density = 1.225
            Viscocity = 0.0000178

            SimulationSteps = 25
            StructuralSettings.StructuralLinkingStep = 5
            UseGpu = False
            GpuDeviceId = 0
            StructuralSettings.ModalDamping = 0.05

            Interval = 0.01
            Cutoff = 0.0001
            CalculateCutoff = False
            ExtendWakes = False

            InertialBasis.U.X = 1.0
            InertialBasis.U.Y = 0.0
            InertialBasis.U.Z = 0.0
            InertialBasis.V.X = 0.0
            InertialBasis.V.Y = 1.0
            InertialBasis.V.Z = 0.0
            InertialBasis.W.X = 0.0
            InertialBasis.W.Y = 0.0
            InertialBasis.W.Z = 1.0

        End Sub
        '------------------------------------------------------------------------------------------



        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Copies the object content into another one (deep copy).
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub Assign(ByVal SimuData As SimulationSettings)

            AnalysisType = SimuData.AnalysisType
            Cutoff = SimuData.Cutoff
            StreamVelocity.Assign(SimuData.StreamVelocity)
            StreamRotation.Assign(SimuData.StreamRotation)
            SimulationSteps = SimuData.SimulationSteps
            Interval = SimuData.Interval
            Viscocity = SimuData.Viscocity
            Density = SimuData.Density
            StaticPressure = SimuData.StaticPressure
            SurveyTolerance = SimuData.SurveyTolerance
            UseGpu = SimuData.UseGpu
            GpuDeviceId = SimuData.GpuDeviceId
            ExtendWakes = SimuData.ExtendWakes
            UnsteadyVelocity.Assign(SimuData.UnsteadyVelocity)
            StructuralSettings.Assign(SimuData.StructuralSettings)

            If Not IsNothing(SimuData.AeroelasticHistogram) Then
                AeroelasticHistogram = SimuData.AeroelasticHistogram.Clone
            Else
                AeroelasticHistogram = Nothing
            End If

        End Sub
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The current value of the dynamic pressure in Pa.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property DynamicPressure As Double
            Get
                Return 0.5 * StreamVelocity.SquareEuclideanNorm * Me.Density
            End Get
        End Property
        '------------------------------------------------------------------------------------------

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The current Reynolds number for a lenght of 1m.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property UnitReynoldsNumber
            Get
                Return StreamVelocity.Norm2 * Density / Viscocity
            End Get
        End Property
        '------------------------------------------------------------------------------------------

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The Reynods number that marks the transition from laminar to turbulent
        ' boundary layers in fuselages.
        ' VGL is based in a very basic approach for skin drag computation in fuselages.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property TransitionReynods As Double = 2000000

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the aproximated sking friction of thick bodies must be included
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property IncludeAproximateBodyFriction As Boolean = False

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the induced drag should be included in the calculation
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property IncludeInducedDrag As Boolean = True




        '==========================================================================================
        ' Generates a vector containing the velocity at each time step.
        ' This is only intended for unsteady or aeroelastic problems.
        '==========================================================================================
        Public Sub GenerateVelocityHistogram()

            Select Case AnalysisType

                Case Settings.CalculationType.FreeFlight
                    UnsteadyVelocity.BaseVelocity.Assign(StreamVelocity)
                    UnsteadyVelocity.GeneratePerturbation(SimulationSteps)

                Case Settings.CalculationType.Aeroelastic
                    If Not IsNothing(AeroelasticHistogram) Then
                        AeroelasticHistogram.Generate(StreamVelocity, Interval, SimulationSteps)
                    End If

            End Select

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Writes the simulation settings in an XML node.
        '==========================================================================================
        Public Sub SaveToXML(ByRef Stream As XmlWriter)

            Stream.WriteStartElement("StreamVelocity")
            Stream.WriteAttributeString("X", String.Format("{0}", StreamVelocity.X))
            Stream.WriteAttributeString("Y", String.Format("{0}", StreamVelocity.Y))
            Stream.WriteAttributeString("Z", String.Format("{0}", StreamVelocity.Z))
            Stream.WriteEndElement()

            Stream.WriteStartElement("StreamOmega")
            Stream.WriteAttributeString("X", String.Format("{0}", StreamRotation.X))
            Stream.WriteAttributeString("Y", String.Format("{0}", StreamRotation.Y))
            Stream.WriteAttributeString("Z", String.Format("{0}", StreamRotation.Z))
            Stream.WriteEndElement()

            Stream.WriteStartElement("Parameters")
            Stream.WriteAttributeString("Analysis", String.Format("{0:D}", AnalysisType))
            Stream.WriteAttributeString("Interval", String.Format("{0}", Interval))
            Stream.WriteAttributeString("Steps", String.Format("{0}", SimulationSteps))
            Stream.WriteAttributeString("Cutoff", String.Format("{0}", Cutoff))
            Stream.WriteAttributeString("SurveyTolerance", String.Format("{0}", SurveyTolerance))
            Stream.WriteAttributeString("ExtendWakes", String.Format("{0}", ExtendWakes))
            Stream.WriteAttributeString("UseGpu", String.Format("{0}", UseGpu))
            Stream.WriteAttributeString("GpuDeviceId", String.Format("{0}", GpuDeviceId))
            Stream.WriteEndElement()

            Stream.WriteStartElement("Fluid")
            Stream.WriteAttributeString("Density", String.Format("{0}", Density))
            Stream.WriteAttributeString("Viscocity", String.Format("{0}", Viscocity))
            Stream.WriteAttributeString("Po", String.Format("{0}", StaticPressure))
            Stream.WriteEndElement()

            Stream.WriteStartElement("Inertia")
            Stream.WriteAttributeString("Mass", String.Format("{0}", Mass))
            Stream.WriteAttributeString("Cgx", String.Format("{0}", CenterOfGravity.X))
            Stream.WriteAttributeString("Cgy", String.Format("{0}", CenterOfGravity.Y))
            Stream.WriteAttributeString("Cgz", String.Format("{0}", CenterOfGravity.Z))
            Stream.WriteAttributeString("Ixx", String.Format("{0}", Ixx))
            Stream.WriteAttributeString("Iyy", String.Format("{0}", Iyy))
            Stream.WriteAttributeString("Izz", String.Format("{0}", Izz))
            Stream.WriteAttributeString("Ux", String.Format("{0}", InertialBasis.U.X))
            Stream.WriteAttributeString("Uy", String.Format("{0}", InertialBasis.U.Y))
            Stream.WriteAttributeString("Uz", String.Format("{0}", InertialBasis.U.Z))
            Stream.WriteAttributeString("Vx", String.Format("{0}", InertialBasis.V.X))
            Stream.WriteAttributeString("Vy", String.Format("{0}", InertialBasis.V.Y))
            Stream.WriteAttributeString("Vz", String.Format("{0}", InertialBasis.V.Z))
            Stream.WriteAttributeString("Wx", String.Format("{0}", InertialBasis.W.X))
            Stream.WriteAttributeString("Wy", String.Format("{0}", InertialBasis.W.Y))
            Stream.WriteAttributeString("Wz", String.Format("{0}", InertialBasis.W.Z))
            Stream.WriteEndElement()

            Stream.WriteStartElement("Structure")
            Stream.WriteAttributeString("StructureStartStep", String.Format("{0}", StructuralSettings.StructuralLinkingStep))
            Stream.WriteAttributeString("Modes", String.Format("{0}", StructuralSettings.NumberOfModes))
            Stream.WriteAttributeString("ModalDamping", String.Format("{0}", StructuralSettings.ModalDamping))
            Stream.WriteAttributeString("SubSteps", String.Format("{0}", StructuralSettings.SubSteps))
            Stream.WriteEndElement()

            If Not IsNothing(UnsteadyVelocity) Then
                Stream.WriteStartElement("VelocityProfile")
                UnsteadyVelocity.SaveToXML(Stream)
                Stream.WriteEndElement()
            End If

            If Not IsNothing(AeroelasticHistogram) Then
                Stream.WriteStartElement("AeroelasticHistogram")
                AeroelasticHistogram.SaveToXML(Stream)
                Stream.WriteEndElement()
            End If

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the simulation settings from an XML node.
        '==========================================================================================
        Public Sub ReadFromXML(ByRef Stream As XmlReader)

            While Stream.Read

                If Stream.NodeType = XmlNodeType.Element Then

                    Select Case Stream.Name

                        Case "StreamVelocity"
                            StreamVelocity.X = IOXML.ReadDouble(Stream, "X", 1.0)
                            StreamVelocity.Y = IOXML.ReadDouble(Stream, "Y", 0.0)
                            StreamVelocity.Z = IOXML.ReadDouble(Stream, "Z", 0.0)

                        Case "StreamOmega"
                            StreamRotation.X = IOXML.ReadDouble(Stream, "X", 0.0)
                            StreamRotation.Y = IOXML.ReadDouble(Stream, "Y", 0.0)
                            StreamRotation.Z = IOXML.ReadDouble(Stream, "Z", 0.0)

                        Case "Parameters"
                            AnalysisType = IOXML.ReadInteger(Stream, "Analysis", CalculationType.SteadyState)
                            Interval = IOXML.ReadDouble(Stream, "Interval", 0.1)
                            SimulationSteps = IOXML.ReadInteger(Stream, "Steps", 15)
                            Cutoff = IOXML.ReadDouble(Stream, "Cutoff", 0.0001)
                            SurveyTolerance = IOXML.ReadDouble(Stream, "SurveyTolerance", 0.001)
                            ExtendWakes = IOXML.ReadBoolean(Stream, "ExtendWakes", False)
                            UseGpu = IOXML.ReadBoolean(Stream, "UseGpu", False)
                            GpuDeviceId = IOXML.ReadInteger(Stream, "GpuDeviceId", 0)

                        Case "Fluid"
                            Density = IOXML.ReadDouble(Stream, "Density", 1.225)
                            Viscocity = IOXML.ReadDouble(Stream, "Viscocity", 0.0000178)
                            StaticPressure = IOXML.ReadDouble(Stream, "Po", 101300)

                        Case "Inertia"

                            Mass = IOXML.ReadDouble(Stream, "Mass", 0.0)
                            CenterOfGravity.X = IOXML.ReadDouble(Stream, "Cgx", 0.0)
                            CenterOfGravity.Y = IOXML.ReadDouble(Stream, "Cgy", 0.0)
                            CenterOfGravity.Z = IOXML.ReadDouble(Stream, "Cgz", 0.0)
                            Ixx = IOXML.ReadDouble(Stream, "Ixx", 0.0)
                            Iyy = IOXML.ReadDouble(Stream, "Iyy", 0.0)
                            Izz = IOXML.ReadDouble(Stream, "Izz", 0.0)
                            InertialBasis.U.X = IOXML.ReadDouble(Stream, "Ux", 1.0)
                            InertialBasis.U.Y = IOXML.ReadDouble(Stream, "Uy", 0.0)
                            InertialBasis.U.Z = IOXML.ReadDouble(Stream, "Uz", 0.0)
                            InertialBasis.V.X = IOXML.ReadDouble(Stream, "Vx", 0.0)
                            InertialBasis.V.Y = IOXML.ReadDouble(Stream, "Vy", 1.0)
                            InertialBasis.V.Z = IOXML.ReadDouble(Stream, "Vz", 0.0)
                            InertialBasis.W.X = IOXML.ReadDouble(Stream, "Wx", 0.0)
                            InertialBasis.W.Y = IOXML.ReadDouble(Stream, "Wy", 0.0)
                            InertialBasis.W.Z = IOXML.ReadDouble(Stream, "Wz", 1.0)

                        Case "Structure"
                            StructuralSettings.NumberOfModes = IOXML.ReadInteger(Stream, "Modes", 6)
                            StructuralSettings.StructuralLinkingStep = IOXML.ReadInteger(Stream, "StructureStartStep", 10)
                            StructuralSettings.ModalDamping = IOXML.ReadDouble(Stream, "ModalDamping", 0.05)
                            StructuralSettings.SubSteps = IOXML.ReadInteger(Stream, "SubSteps", 1)

                        Case "VelocityProfile"
                            UnsteadyVelocity.ReadFromXML(Stream.ReadSubtree)

                        Case "AeroelasticHistogram"
                            AeroelasticHistogram.ReadFromXML(Stream.ReadSubtree)

                    End Select

                End If

            End While

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Writes the settings to an XML file 
        '==========================================================================================
        Public Sub WriteToXML(FilePath As String)

            Dim Stream As XmlWriter = XmlWriter.Create(FilePath)

            Stream.WriteStartElement("Settings")

            SaveToXML(Stream)

            Stream.WriteEndElement()

            Stream.Close()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Reads the settings from an XML file 
        '==========================================================================================
        Public Sub ReadFromXML(FilePath As String)

            If IO.File.Exists(FilePath) Then

                Dim Reader As XmlReader = XmlReader.Create(FilePath)

                ReadFromXML(Reader)

            End If

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Assigns the standard atmosphere to the fluid properties
        '==========================================================================================
        Public Sub AssignStandardAtmosphere(Altitude As Double)

            Dim ISA As New StandardAtmosphere(Altitude)

            Density = ISA.Density
            StaticPressure = ISA.Pressure
            Viscocity = ISA.KinematicVisc

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        '
        '==========================================================================================
        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '----------------------------------------------------------------------------------------------
    
End Namespace
'--------------------------------------------------------------------------------------------------
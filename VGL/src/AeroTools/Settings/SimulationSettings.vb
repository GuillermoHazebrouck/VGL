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

    ''' <summary>
    ''' A sence bit.
    ''' </summary>
    Public Enum Sence As Integer
        Positive = 1
        Negative = -1
    End Enum

    ''' <summary>
    ''' The different options to solve the linear equations.
    ''' </summary>
    Public Enum MatrixSolverType As Byte
        LU = 0
        QR = 1
    End Enum

    ''' <summary>
    ''' Enumerates the adjacent rings on a given ring.
    ''' </summary>
    Public Enum AdjacentRing As Byte
        Panel1 = 0
        Panel2 = 1
        Panel3 = 2
        Panel4 = 3
    End Enum

    ''' <summary>
    ''' The possible kind of simulation.
    ''' </summary>
    Public Enum CalculationType As Byte
        SteadyState = 0
        FreeFlight = 1
        Aeroelastic = 2
    End Enum

    ''' <summary>
    ''' Gathers all settings necessary for the calculation.
    ''' </summary>
    Public Class SimulationSettings

        ''' <summary>
        ''' Free stream velocity vector in m/s.
        ''' </summary>
        ''' <remarks>
        ''' The velocity components are scaled by an amplitude factor in unsteady problems.
        ''' </remarks>
        Public Property StreamVelocity As New Vector3

        ''' <summary>
        ''' Angular velocity of the aircraft reference frame in rad/s.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property StreamRotation As New Vector3

        ''' <summary>
        ''' Free stream density in kg/m³.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property Density As Double = 1.225#

        ''' <summary>
        ''' Free stream static pressure in Pa.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property StaticPressure As Double = 101300.0#

        ''' <summary>
        ''' Free stream viscosity in kg/(m.s).
        ''' </summary>
        ''' <remarks></remarks>
        Public Property Viscocity As Double = 0.0000178#

        ''' <summary>
        ''' Specifies the number of integration steps.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property SimulationSteps As Integer = 1

        ''' <summary>
        ''' The gravity intensity (assumed in the global Z direction)
        ''' </summary>
        ''' <returns></returns>
        Public Property Gravity As Double = -9.8

        ''' <summary>
        ''' The model mass in kg
        ''' </summary>
        ''' <returns></returns>
        Public Property Mass As Double = 0.0#

        ''' <summary>
        ''' The model center of gravity
        ''' </summary>
        ''' <returns></returns>
        Public Property CenterOfGravity As New Vector3

        ''' <summary>
        ''' The model second moment of inertia about the X axis in kg.m²
        ''' </summary>
        ''' <returns></returns>
        Public Property Ixx As Double = 0.0#

        ''' <summary>
        ''' The model second moment of inertia about the Y axis  in kg.m²
        ''' </summary>
        ''' <returns></returns>
        Public Property Iyy As Double = 0.0#

        ''' <summary>
        ''' The model second moment of inerti about the Z axis a in kg.m²
        ''' </summary>
        ''' <returns></returns>
        Public Property Izz As Double = 0.0#

        ''' <summary>
        ''' The main directions of inertia
        ''' </summary>
        ''' <returns></returns>
        Public Property InertialBasis As New Base3

        ''' <summary>
        ''' The number of rigid steps in a free flight simulation
        ''' </summary>
        ''' <returns></returns>
        Public Property FreeFlightStartStep As Integer = 50

        ''' <summary>
        ''' The number of correction steps in a free flight simulation
        ''' </summary>
        ''' <returns></returns>
        Public Property CorrectionSteps As Integer = 10

        ''' <summary>
        ''' Specifies the size of the instegration step in seconds.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property Interval As Double = 0.1

        ''' <summary>
        ''' Specifies the radius of the region around vortices where the velocity is null in meters.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property Cutoff As Double = 0.0001

        ''' <summary>
        ''' Specifies whether the cutoff has to be automatically estimated based on the mesh.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property CalculateCutoff As Boolean

        ''' <summary>
        ''' Contains the necessary structural settings.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property StructuralSettings As New Models.Structural.Library.StructuralSettings

        ''' <summary>
        ''' Amplitude of the free stream velocity components for every time step in m/s.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property UnsteadyVelocity As New UnsteadyVelocity

        ''' <summary>
        ''' Contains information about how the simulation parameters vary during an aeroelastic analysis.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property AeroelasticHistogram As New AeroelasticHistogram

        ''' <summary>
        ''' The type of analysis to be performed.
        ''' </summary>
        ''' <returns></returns>
        Public Property AnalysisType As CalculationType = CalculationType.SteadyState

        ''' <summary>
        ''' Maximum distance between two rings to be considered as adjacent, in meters.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property SurveyTolerance As Double = 0.001

        ''' <summary>
        ''' Indicates if the influence of the wakes on the fuselage should be included.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property StrongWakeInfluence As Boolean = False

        ''' <summary>
        ''' Indicates if the GPU should be used for some of the computations.
        ''' </summary>
        ''' <returns></returns>
        Public Property UseGpu As Boolean = False

        ''' <summary>
        ''' The id of the Gpu device to use (in case the UseGpu is true).
        ''' </summary>
        ''' <returns></returns>
        Public Property GpuDeviceId As Integer = 0

        ''' <summary>
        ''' Indicates if the wakes must be extended in the stream direction after the trimming step.
        ''' </summary>
        ''' <returns>
        ''' The current extension is 100m, and cannot be adapted.
        ''' </returns>
        Public Property ExtendWakes As Boolean = False

        ''' <summary>
        ''' Sets the default values.
        ''' </summary>
        ''' <remarks>
        ''' The default values correspond to the standard atmosphere at sea level.
        ''' </remarks>
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

        ''' <summary>
        ''' Copies the object content into another one (deep copy).
        ''' </summary>
        ''' <param name="SimuData"></param>
        ''' <remarks></remarks>
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

        ''' <summary>
        ''' The current value of the dynamic pressure in Pa.
        ''' </summary>
        ''' <returns></returns>
        Public ReadOnly Property DynamicPressure As Double
            Get
                Return 0.5 * StreamVelocity.SquareEuclideanNorm * Me.Density
            End Get
        End Property

        ''' <summary>
        ''' The current Reynolds number for a lenght of 1m.
        ''' </summary>
        ''' <returns></returns>
        Public ReadOnly Property UnitReynoldsNumber
            Get
                Return StreamVelocity.Norm2 * Density / Viscocity
            End Get
        End Property

        ''' <summary>
        ''' The Reynods number that marks the transition from laminar to turbulent
        ''' boundary layers in fuselages.
        ''' </summary>
        ''' <returns>
        ''' VGL is based in a very basic approach for skin drag computation in fuselages.
        ''' </returns>
        Public Property TransitionReynods As Double = 2000000

        ''' <summary>
        ''' Indicates if the aproximated sking friction of thick bodies must be included
        ''' </summary>
        ''' <returns></returns>
        Public Property IncludeAproximateBodyFriction As Boolean = False

        ''' <summary>
        ''' Indicates if the induced drag should be included in the calculation
        ''' </summary>
        ''' <returns></returns>
        Public Property IncludeInducedDrag As Boolean = True

        ''' <summary>
        ''' Generates a vector containing the velocity at each time step.
        ''' </summary>
        ''' <remarks>
        ''' This is only intended for unsteady or aeroelastic problems.
        ''' </remarks>
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

        ''' <summary>
        ''' Writes the simulation settings in an XML node.
        ''' </summary>
        ''' <param name="writer"></param>
        Public Sub SaveToXML(ByRef writer As XmlWriter)

            writer.WriteStartElement("StreamVelocity")
            writer.WriteAttributeString("X", String.Format("{0}", StreamVelocity.X))
            writer.WriteAttributeString("Y", String.Format("{0}", StreamVelocity.Y))
            writer.WriteAttributeString("Z", String.Format("{0}", StreamVelocity.Z))
            writer.WriteEndElement()

            writer.WriteStartElement("StreamOmega")
            writer.WriteAttributeString("X", String.Format("{0}", StreamRotation.X))
            writer.WriteAttributeString("Y", String.Format("{0}", StreamRotation.Y))
            writer.WriteAttributeString("Z", String.Format("{0}", StreamRotation.Z))
            writer.WriteEndElement()

            writer.WriteStartElement("Parameters")
            writer.WriteAttributeString("Analysis", String.Format("{0:D}", AnalysisType))
            writer.WriteAttributeString("Interval", String.Format("{0}", Interval))
            writer.WriteAttributeString("Steps", String.Format("{0}", SimulationSteps))
            writer.WriteAttributeString("Cutoff", String.Format("{0}", Cutoff))
            writer.WriteAttributeString("SurveyTolerance", String.Format("{0}", SurveyTolerance))
            writer.WriteAttributeString("ExtendWakes", String.Format("{0}", ExtendWakes))
            writer.WriteAttributeString("UseGpu", String.Format("{0}", UseGpu))
            writer.WriteAttributeString("GpuDeviceId", String.Format("{0}", GpuDeviceId))
            writer.WriteEndElement()

            writer.WriteStartElement("Fluid")
            writer.WriteAttributeString("Density", String.Format("{0}", Density))
            writer.WriteAttributeString("Viscocity", String.Format("{0}", Viscocity))
            writer.WriteAttributeString("Po", String.Format("{0}", StaticPressure))
            writer.WriteEndElement()

            writer.WriteStartElement("Inertia")
            writer.WriteAttributeString("Mass", String.Format("{0}", Mass))
            writer.WriteAttributeString("Cgx", String.Format("{0}", CenterOfGravity.X))
            writer.WriteAttributeString("Cgy", String.Format("{0}", CenterOfGravity.Y))
            writer.WriteAttributeString("Cgz", String.Format("{0}", CenterOfGravity.Z))
            writer.WriteAttributeString("Ixx", String.Format("{0}", Ixx))
            writer.WriteAttributeString("Iyy", String.Format("{0}", Iyy))
            writer.WriteAttributeString("Izz", String.Format("{0}", Izz))
            writer.WriteAttributeString("Ux", String.Format("{0}", InertialBasis.U.X))
            writer.WriteAttributeString("Uy", String.Format("{0}", InertialBasis.U.Y))
            writer.WriteAttributeString("Uz", String.Format("{0}", InertialBasis.U.Z))
            writer.WriteAttributeString("Vx", String.Format("{0}", InertialBasis.V.X))
            writer.WriteAttributeString("Vy", String.Format("{0}", InertialBasis.V.Y))
            writer.WriteAttributeString("Vz", String.Format("{0}", InertialBasis.V.Z))
            writer.WriteAttributeString("Wx", String.Format("{0}", InertialBasis.W.X))
            writer.WriteAttributeString("Wy", String.Format("{0}", InertialBasis.W.Y))
            writer.WriteAttributeString("Wz", String.Format("{0}", InertialBasis.W.Z))
            writer.WriteEndElement()

            writer.WriteStartElement("Structure")
            writer.WriteAttributeString("StructureStartStep", String.Format("{0}", StructuralSettings.StructuralLinkingStep))
            writer.WriteAttributeString("Modes", String.Format("{0}", StructuralSettings.NumberOfModes))
            writer.WriteAttributeString("ModalDamping", String.Format("{0}", StructuralSettings.ModalDamping))
            writer.WriteAttributeString("SubSteps", String.Format("{0}", StructuralSettings.SubSteps))
            writer.WriteEndElement()

            If Not IsNothing(UnsteadyVelocity) Then
                writer.WriteStartElement("VelocityProfile")
                UnsteadyVelocity.SaveToXML(writer)
                writer.WriteEndElement()
            End If

            If Not IsNothing(AeroelasticHistogram) Then
                writer.WriteStartElement("AeroelasticHistogram")
                AeroelasticHistogram.SaveToXML(writer)
                writer.WriteEndElement()
            End If

        End Sub

        ''' <summary>
        ''' Reads the simulation settings from an XML node.
        ''' </summary>
        ''' <param name="reader"></param>
        Public Sub ReadFromXML(ByRef reader As XmlReader)

            While reader.Read

                If reader.NodeType = XmlNodeType.Element Then

                    Select Case reader.Name

                        Case "StreamVelocity"
                            StreamVelocity.X = IOXML.ReadDouble(reader, "X", 1.0)
                            StreamVelocity.Y = IOXML.ReadDouble(reader, "Y", 0.0)
                            StreamVelocity.Z = IOXML.ReadDouble(reader, "Z", 0.0)

                        Case "StreamOmega"
                            StreamRotation.X = IOXML.ReadDouble(reader, "X", 0.0)
                            StreamRotation.Y = IOXML.ReadDouble(reader, "Y", 0.0)
                            StreamRotation.Z = IOXML.ReadDouble(reader, "Z", 0.0)

                        Case "Parameters"
                            AnalysisType = IOXML.ReadInteger(reader, "Analysis", CalculationType.SteadyState)
                            Interval = IOXML.ReadDouble(reader, "Interval", 0.1)
                            SimulationSteps = IOXML.ReadInteger(reader, "Steps", 15)
                            Cutoff = IOXML.ReadDouble(reader, "Cutoff", 0.0001)
                            SurveyTolerance = IOXML.ReadDouble(reader, "SurveyTolerance", 0.001)
                            ExtendWakes = IOXML.ReadBoolean(reader, "ExtendWakes", False)
                            UseGpu = IOXML.ReadBoolean(reader, "UseGpu", False)
                            GpuDeviceId = IOXML.ReadInteger(reader, "GpuDeviceId", 0)

                        Case "Fluid"
                            Density = IOXML.ReadDouble(reader, "Density", 1.225)
                            Viscocity = IOXML.ReadDouble(reader, "Viscocity", 0.0000178)
                            StaticPressure = IOXML.ReadDouble(reader, "Po", 101300)

                        Case "Inertia"

                            Mass = IOXML.ReadDouble(reader, "Mass", 0.0)
                            CenterOfGravity.X = IOXML.ReadDouble(reader, "Cgx", 0.0)
                            CenterOfGravity.Y = IOXML.ReadDouble(reader, "Cgy", 0.0)
                            CenterOfGravity.Z = IOXML.ReadDouble(reader, "Cgz", 0.0)
                            Ixx = IOXML.ReadDouble(reader, "Ixx", 0.0)
                            Iyy = IOXML.ReadDouble(reader, "Iyy", 0.0)
                            Izz = IOXML.ReadDouble(reader, "Izz", 0.0)
                            InertialBasis.U.X = IOXML.ReadDouble(reader, "Ux", 1.0)
                            InertialBasis.U.Y = IOXML.ReadDouble(reader, "Uy", 0.0)
                            InertialBasis.U.Z = IOXML.ReadDouble(reader, "Uz", 0.0)
                            InertialBasis.V.X = IOXML.ReadDouble(reader, "Vx", 0.0)
                            InertialBasis.V.Y = IOXML.ReadDouble(reader, "Vy", 1.0)
                            InertialBasis.V.Z = IOXML.ReadDouble(reader, "Vz", 0.0)
                            InertialBasis.W.X = IOXML.ReadDouble(reader, "Wx", 0.0)
                            InertialBasis.W.Y = IOXML.ReadDouble(reader, "Wy", 0.0)
                            InertialBasis.W.Z = IOXML.ReadDouble(reader, "Wz", 1.0)

                        Case "Structure"
                            StructuralSettings.NumberOfModes = IOXML.ReadInteger(reader, "Modes", 6)
                            StructuralSettings.StructuralLinkingStep = IOXML.ReadInteger(reader, "StructureStartStep", 10)
                            StructuralSettings.ModalDamping = IOXML.ReadDouble(reader, "ModalDamping", 0.05)
                            StructuralSettings.SubSteps = IOXML.ReadInteger(reader, "SubSteps", 1)

                        Case "VelocityProfile"
                            UnsteadyVelocity.ReadFromXML(reader.ReadSubtree)

                        Case "AeroelasticHistogram"
                            AeroelasticHistogram.ReadFromXML(reader.ReadSubtree)

                    End Select

                End If

            End While

        End Sub

        ''' <summary>
        ''' Writes the settings to an XML file 
        ''' </summary>
        ''' <param name="FilePath"></param>
        Public Sub WriteToXML(FilePath As String)

            Dim writer As XmlWriter = XmlWriter.Create(FilePath)

            writer.WriteStartElement("Settings")

            SaveToXML(writer)

            writer.WriteEndElement()

            writer.Close()

        End Sub

        ''' <summary>
        ''' Reads the settings from an XML file 
        ''' </summary>
        ''' <param name="FilePath"></param>
        Public Sub ReadFromXML(FilePath As String)

            If IO.File.Exists(FilePath) Then

                Dim Reader As XmlReader = XmlReader.Create(FilePath)

                ReadFromXML(Reader)

            End If

        End Sub

        ''' <summary>
        ''' Assigns the standard atmosphere to the fluid properties
        ''' </summary>
        ''' <param name="Altitude"></param>
        Public Sub AssignStandardAtmosphere(Altitude As Double)

            Dim ISA As New StandardAtmosphere(Altitude)

            Density = ISA.Density
            StaticPressure = ISA.Pressure
            Viscocity = ISA.KinematicVisc

        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

    End Class

End Namespace
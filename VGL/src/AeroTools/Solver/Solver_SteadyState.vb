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
Imports DotNumerics.LinearAlgebra
Imports VGL.AeroTools.Settings
Imports VGL.MathTools.Algebra.EuclideanSpace

'#############################################################################
' Unit: Solver_SteadyState
'
' This unit is the part of the kernel that does the steady state simulation.
'#############################################################################
Namespace AeroTools.Solver

    Partial Public Class Solver

        ''' <summary>
        ''' This method simulates an experiment where the model is rigidly 
        ''' attached to the inertial reference frame and the flow is
        ''' suddently started for a given numer of time steps. If the
        ''' number of steps is sufficiently high, the steady state
        ''' will be reached.
        ''' Loads are only calculated at the last time step.
        ''' </summary>
        Public Sub SteadyStateTransit(ByVal ReferenceFilePath As String)

            RaiseEvent PushMessage("Starting steady analysis")
            RaiseEvent PushMessage("Solver version: " & Version)

            '///////////////////////////////'
            ' Initialize output directories '
            '///////////////////////////////'

            CreateSubFolder(CalculationType.SteadyState, ReferenceFilePath)

            CheckForSources()

            Dim StartingTime As Date = Now

            '//////////////////////////////'
            ' Initialize stream properties '
            '//////////////////////////////'

            Stream.Velocity.Assign(Settings.StreamVelocity)
            Stream.Rotation.Assign(Settings.StreamRotation)
            Stream.Density = Settings.Density
            Stream.SquareVelocity = Stream.Velocity.SquareEuclideanNorm
            Stream.DynamicPressure = 0.5 * Stream.Density * Stream.SquareVelocity
            WithStreamRotation = Stream.Rotation.Norm2 > 0

            Dim WakeExtension As New Vector3(Settings.StreamVelocity)
            WakeExtension.Normalize()
            WakeExtension.Scale(100.0)

            '////////////////////////////////////'
            ' Build influcence matrix (constant) '
            '////////////////////////////////////'

            RaiseEvent PushMessage("Building doublets matrix")
            BuildMatrixForDoublets()

            If WithSources Then

                RaiseEvent PushMessage("Building sources matrix")
                BuildMatrixForSources()

                RaiseEvent PushMessage("Assigning sources")
                AssignSources()

            End If

            '////////////////'
            ' Initialize RHS '
            '////////////////'

            RaiseEvent PushMessage("Building RHS")
            BuildRightHandSide1()

            RaiseEvent PushMessage("Initializing wakes")
            InitializeWakes()

            RaiseEvent PushMessage(String.Format("Generating LU decomposition ({0})", Dimension))
            Dim Equations As New LinearEquations
            Equations.ComputeLU(MatrixDoublets)
            G = New Vector(Dimension)

            '///////////////////////'
            ' Generate relaxed wake '
            '///////////////////////'

            For TimeStep = 1 To Settings.SimulationSteps

                If CancellationPending Then
                    CancelProcess()
                    Return
                End If

                RaiseEvent PushProgress(String.Format("Step {0}", TimeStep), 100 * TimeStep / Settings.SimulationSteps)

                '/////////////////////////////////////'
                ' Find circulation on bouded lattices '
                '/////////////////////////////////////'

                Equations.SolveLU(RHS, G)

                AssignDoublets()

                '//////////////'
                ' Convect wake '
                '//////////////'

                CalculateVelocityOnWakes()

                For Each Lattice In Lattices

                    If WithSources And Settings.StrongWakeInfluence Then

                        Lattice.PopulateWakeRingsAndVortices(Settings.Interval, TimeStep, Settings.ExtendWakes, WakeExtension)

                    Else

                        ' NOTE: the root vortex is tamed when there are fuselages to simulate continuity in the circulation.

                        Lattice.PopulateWakeVortices(Settings.Interval, TimeStep, Settings.ExtendWakes, WakeExtension)

                    End If

                Next

                '/////////////////'
                ' Rebuild the RHS '
                '/////////////////'

                CalculateVelocityInducedByTheWakesOnBoundedLattices(True)

                If WithSources And Settings.StrongWakeInfluence Then

                    CalculatePotentialInducedByTheWakeOnThickBoundedLattices()

                End If

                BuildRightHandSide2()

            Next

            RaiseEvent PushMessage("Calculating airloads")

            '////////////////////////'
            ' Complete the last step '
            '////////////////////////'

            Equations.SolveLU(RHS, G)

            AssignDoublets()

            '//////////////////////////////////'
            ' Calculate vortex rings Cp or DCp '
            '//////////////////////////////////'

            CalculateVelocityInducedByTheWakesOnBoundedLattices(False)

            CalculateTotalVelocityOnBoundedLattices()

            For Each Lattice In Lattices

                Lattice.CalculatePressure(Stream.SquareVelocity)

            Next

            '/////////////////////////////'
            ' Calculate the total airload '
            '/////////////////////////////'

            CalculateAirloads()

            '///////////////////////////'
            ' Announce ready and finish '
            '///////////////////////////'

            RaiseEvent PushMessage("Writing results data...")

            ' Lattices from the last step (frame 0)
            '------------------------------------------
            WriteLattices(BaseDirectoryPath, 0)

            ' Settings
            '------------------------------------------
            Settings.WriteToXML(IO.Path.Combine(BaseDirectoryPath, "Settings.xml"))

            ' Polars
            '------------------------------------------
            PolarDataBase.WriteBinary(IO.Path.Combine(BaseDirectoryPath, "Polars.bin"))

            ' Info
            '------------------------------------------
            WriteInfoFile(BaseDirectoryPath, CalculationType.SteadyState)

            Dim Interval As TimeSpan = Now - StartingTime
            Dim Message As String = String.Format("Calculation finished. Elapsed time: {0}m {1}.{2}s", Interval.Minutes, Interval.Seconds, Interval.Milliseconds)
            RaiseEvent PushMessage(Message)

            RaiseEvent CalculationDone()

        End Sub

    End Class

End Namespace


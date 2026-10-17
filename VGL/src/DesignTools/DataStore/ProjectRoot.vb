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

'' Standard .NET frameworks dependencies
'-----------------------------------------------------------------------------
Imports System.Xml

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.AeroTools.Solver
Imports VGL.AeroTools.Settings
Imports VGL.DesignTools.Models
Imports VGL.DesignTools.Models.Components

'#############################################################################
' Unit: ProjectRoot
'
' This unit provides the static storage of the project data, including the
' model and the results.
'#############################################################################
Namespace DesignTools.DataStore

    Public Module ProjectRoot

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The name of the project
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Name As String = "Aircraft"

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The main project file
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property FilePath As String = ""

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The settings used to run the simulation
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property SimulationSettings As New SimulationSettings

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The complete design model made of high level components
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Model As DesignModel = New DesignModel

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The result model
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Results As New ResultModel

        Private _Initialized As Boolean = False

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if the project has been initialized
        '//////////////////////////////////////////////////////////////////////////////////////////
        ReadOnly Property Initialized As Boolean
            Get
                Return _Initialized
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Initializes the module
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub Initialize()

            SimulationSettings.InitializeParameters()
            Results.Clear()
            _Initialized = True

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Restarts the project by reseting the model an clearing the results
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub RestartProject()

            Name = "New aircraft"
            Model.Objects.Clear()
            SimulationSettings.InitializeParameters()
            Results.Clear()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if the current file path exist
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property ExistsOnDatabase As Boolean
            Get
                Return System.IO.File.Exists(FilePath)
            End Get
        End Property

#Region " Basic calculation launcher "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Starts a synchronous calculation process.
        ' This call is used for the console, but is less sutable for an animated GUI.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <param name="Type"></param>
        Public Sub StartCalculation(ByVal Type As CalculationType, ByRef CalculationCore As Solver)

            SimulationSettings.AnalysisType = Type

            Try

                RaiseEvent PushMessage("Preparing calculation cell")
                Dim BuildStructure As Boolean = Type = CalculationType.Aeroelastic
                CalculationCore.GenerateFromExistingModel(Model, SimulationSettings, BuildStructure)

                AddHandler CalculationCore.PushProgress, AddressOf HandleProgress
                AddHandler CalculationCore.PushMessage, AddressOf HandleMessage
                AddHandler CalculationCore.CalculationDone, AddressOf CalculationFinished
                AddHandler CalculationCore.CalculationAborted, AddressOf CalculationAborted

                Dim StartingTime As Date = Now

                RaiseEvent PushMessage("Calculating with parallel solver")

                Select Case Type

                    Case CalculationType.SteadyState

                        CalculationCore.SteadyStateTransit(FilePath)

                    Case CalculationType.FreeFlight

                        CalculationCore.FreeFlight(FilePath)

                    Case CalculationType.Aeroelastic
                        CalculationCore.AeroelasticTransit(FilePath)

                End Select

            Catch ex As Exception
                RaiseEvent PushMessage(String.Format("Calculation exited with exception: ""{0}"".", ex.Message))
                RaiseEvent PushMessage("Exited with exception!")
                RaiseEvent CalculationDone()
                Return
            End Try

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Handles the calculation finished of the solver
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private Sub CalculationFinished()

            RaiseEvent PushMessage("Ready")
            RaiseEvent PushMessage("Calculation done")
            RaiseEvent CalculationDone()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Handles the calculation aborted of the solver
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private Sub CalculationAborted()

            RaiseEvent PushMessage("Calculation aborted")
            RaiseEvent CalculationDone()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Occurs when the calculation finishes.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Event CalculationDone()

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Occurs when the calculation finishes.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Event PushProgress(Title As String, Value As Integer)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Occurs when the calculation finishes.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Event PushMessage(Message As String)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Handles the progress of the solver
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub HandleProgress(Title As String, Value As Integer)

            RaiseEvent PushProgress(Title, Value)

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Handles the messages of the solver
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub HandleMessage(Message As String)

            RaiseEvent PushMessage(Message)

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

#Region " Input/Output "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads a full project from XML file
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ReadFromXML()

            If Not ExistsOnDatabase Then Exit Sub

            Dim Reader As XmlReader = XmlReader.Create(FilePath)

            If Reader.ReadToDescendant("Project") Then

                ReadFromXML(Reader)

            End If

            Reader.Close()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads a full project from XML subtree
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ReadFromXML(Reader As XmlReader)

            Name = Reader.GetAttribute("Name")

            While Reader.Read

                Select Case Reader.Name

                    Case "Model"
                        Model.ReadFromXML(Reader.ReadSubtree)

                    Case "Simulacion"
                        SimulationSettings.ReadFromXML(Reader.ReadSubtree)

                End Select

            End While

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes the full project to an XML file
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub WriteToXML()

            Dim Writer As XmlWriter = XmlWriter.Create(FilePath)

            Writer.WriteStartElement("VGL")

            Writer.WriteStartElement("Project")
            WriteToXML(Writer)
            Writer.WriteEndElement()

            Writer.WriteEndElement()

            Writer.Close()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes the full project to an XML subtree
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub WriteToXML(Writer As XmlWriter)

            Writer.WriteAttributeString("Name", Name)

            Writer.WriteStartElement("Model")
            Model.WriteToXML(Writer)
            Writer.WriteEndElement()

            Writer.WriteStartElement("Simulacion")
            SimulationSettings.SaveToXML(Writer)
            Writer.WriteEndElement()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Loads the results from an XML results file
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ReadResults(ByVal ReferenceFilePath As String)

            Results.Clear()
            Results.LoadFromDirectory(IO.Path.GetDirectoryName(ReferenceFilePath))

        End Sub
        '------------------------------------------------------------------------------------------

        Public Enum ExportTypes As Integer
            ExportStl
            ExportNative
            ExportScilab
        End Enum

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Exports the model geometry in the given format.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ExportDesignModel(Type As ExportTypes, OneFile As Boolean)

            Dim FileName As String = Strings.Left(FilePath, FilePath.Length - 4)

            Select Case Type

                Case ExportTypes.ExportStl

                    If OneFile Then

                        ' Write all solids in a single file

                        FileName = FileName & ".stl"

                        Dim FileId As Integer = FreeFile()
                        FileOpen(FileId, FileName, OpenMode.Output)
                        PrintLine(FileId, "solid Model")
                        FileClose(FileId)

                        For Each Surface In Model.Objects
                            Surface.ExportStlFile(FileName, True)
                        Next

                        FileId = FreeFile()
                        FileOpen(FileId, FileName, OpenMode.Append)
                        PrintLine(FileId, "endsolid Model")
                        FileClose(FileId)

                    Else

                        ' Write each model surface in a different file

                        Dim I As Integer = 0

                        For Each Surface In Model.Objects

                            Dim Name As String = Surface.Name

                            If Name = "" Then
                                Name = "Surface_" & I
                            End If

                            Surface.ExportStlFile(FileName & "_" & Name & ".stl", False)

                        Next

                    End If

                Case ExportTypes.ExportNative

                    If OneFile Then

                        ' Write all solids in a single file

                        FileName = FileName & ".dat"

                        For Each Surface In Model.Objects
                            Surface.ExportNativeFile(FileName, True)
                        Next

                    Else

                        ' Write each model surface in a different file

                        Dim I As Integer = 0

                        For Each Surface In Model.Objects

                            Dim Name As String = Surface.Name

                            If Name = "" Then
                                Name = "Surface_" & I
                            End If

                            Surface.ExportNativeFile(FileName & "_" & Name & ".dat", False)

                        Next

                    End If

            End Select

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Module

End Namespace







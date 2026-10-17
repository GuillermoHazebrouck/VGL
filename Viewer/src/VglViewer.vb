'#############################################################################
'VGL
'Open source aeromechanics in dotnet
'Copyright (C) 2025 Guillermo Hazebrouck (guillermo.hazebrouck@VGL.org)

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
Imports VGL.AeroTools.Solver
Imports VGL.AeroTools.Settings
Imports VGL.DesignTools
Imports VGL.DesignTools.Models.Components
Imports VGL.DesignTools.Models.Components.Basics
Imports OpenTK.Graphics.OpenGL4
Imports OpenTK.Mathematics
Imports OpenTK.Windowing.Common
Imports OpenTK.Windowing.Desktop
Imports OpenTK.Windowing.GraphicsLibraryFramework

'#############################################################################
' Unit: The VGL viewer is an OpenGL port based on OpenTK
'
'
'#############################################################################
Module VglViewer

    '//////////////////////////////////////////////////////////////////////////////////////////////
    ''' The data on a particular time step
    '//////////////////////////////////////////////////////////////////////////////////////////////
    Public Class VglWindow

        Inherits GameWindow

        Private Model as VisualElements

        '=============================================================================
        '
        '=============================================================================
        Public Sub New()

            MyBase.New(GameWindowSettings.Default, New NativeWindowSettings With {
                                                   .ClientSize = New Vector2i(600, 600),
                                                   .Title = "VGL",
                                                   .Flags = ContextFlags.ForwardCompatible})

            'Me.VSync = VSyncMode.Off
            'Me.TargetRenderFrequency = 10.0

            Model = new VisualElements

        End Sub
        '-----------------------------------------------------------------------------



        '=============================================================================
        '
        '=============================================================================
        Protected Overrides Sub OnUpdateFrame(Arguments As FrameEventArgs)

            MyBase.OnUpdateFrame(Arguments)

            If KeyboardState.IsKeyDown(Keys.Escape) Then

                Close()

            End If

        End Sub
        '-----------------------------------------------------------------------------



        '=============================================================================
        '
        '=============================================================================
        Protected Overrides Sub OnLoad()

            MyBase.OnLoad()

            Model.LoadResources()

            Model.LoadDesignModel()

        End Sub
        '-----------------------------------------------------------------------------



        '=============================================================================
        '
        '=============================================================================
        Protected Overrides Sub OnUnload()

            MyBase.OnUnload()

            Model.ClearResources()

        End Sub
        '-----------------------------------------------------------------------------


        
        Private RefreshFrame As Boolean = True
        Private FirstPostion as Boolean = True
        Private LastPosition as Vector2 = new Vector2(0.0#, 0.0#)
        '=============================================================================
        '
        '=============================================================================
        Protected Overrides Sub OnRenderFrame(Arguments As FrameEventArgs)

            MyBase.OnRenderFrame(Arguments)

            ' Update the view
            '-------------------------------
            Dim Mouse = MouseState

            If FirstPostion Then
                LastPosition.X = Mouse.X
                LastPosition.Y = Mouse.Y
                FirstPostion = False
            Else
                If LastPosition.X <> Mouse.X Or LastPosition.Y <> Mouse.Y Then
                    Dim RotationY = LastPosition.X - Mouse.X
                    Dim RotationZ = LastPosition.Y - Mouse.Y
                    Model.SetTrasform (0.1#, RotationY, RotationZ)
                    RefreshFrame = True
                End If
            End If

            ' Refresh
            '-------------------------------
            If RefreshFrame Then

                GL.ClearColor(0.6#, 0.3#, 0.3#, 1.0#)

                GL.Clear(ClearBufferMask.ColorBufferBit)

                Model.RenderFrame()

                SwapBuffers()

                RefreshFrame = False

            End If

        End Sub

    End Class
    '------------------------------------------------------------------------------------------



    '=============================================================================
    ' The main window
    '=============================================================================
    Public Sub Main

        Dim Window As New VglWindow

        DataStore.ProjectRoot.Initialize()

        Dim Commands As String() = Command.Split({";"c}, StringSplitOptions.RemoveEmptyEntries)

        If Commands.Length > 0 Then

            DataStore.FilePath = Commands(0)

            If DataStore.ProjectRoot.ExistsOnDatabase Then

                DataStore.ProjectRoot.RestartProject()
                DataStore.ProjectRoot.ReadFromXML()
                System.Console.WriteLine(String.Format("loaded {0} objects", DataStore.ProjectRoot.Model.Objects.Count))

                Window.Run()

            End If

        Else
            System.Console.WriteLine("please, enter file name")

        End If

    End Sub
    '-----------------------------------------------------------------------------

End Module
'-----------------------------------------------------------------------------

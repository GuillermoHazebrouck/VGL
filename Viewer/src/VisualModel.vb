Imports System.IO
Imports OpenTK.Graphics.OpenGL4
Imports OpenTK.Mathematics
Imports VGL.DesignTools
Imports VGL.DesignTools.Models.Components
Imports VGL.DesignTools.Models.Components.Basics

'#############################################################################
' Unit: The VGL viewer is an OpenGL port based on OpenTK
'
'
'#############################################################################
Module VisualModel
    
    '//////////////////////////////////////////////////////////////////////////////////////////////
    ''' The shader program to draw surfaces
    '//////////////////////////////////////////////////////////////////////////////////////////////
    Public Class ShaderProgram

        Implements IDisposable

        Private Handle as Integer

        Private ColorUniform as Integer = 0

        Private MatrixUniform as Integer = 0

        '=============================================================================
        '
        '=============================================================================
        Public Sub New (VertexPath as String, FragmentPath as String)

            Console.WriteLine("building shader program")

            Dim VertexShader as Integer = GL.CreateShader(ShaderType.VertexShader)
            Dim VertexShaderSource as String = File.ReadAllText(VertexPath)
            GL.ShaderSource(VertexShader, VertexShaderSource)

            Dim FragmentShader as Integer = GL.CreateShader(ShaderType.FragmentShader)
            Dim FragmentShaderSource as String = File.ReadAllText(FragmentPath)
            GL.ShaderSource(FragmentShader, FragmentShaderSource)

            Dim Success as Integer

            GL.CompileShader(VertexShader)
            GL.GetShader(VertexShader, ShaderParameter.CompileStatus, Success)
            If Success = 0 then
                Dim InfoLog as String = GL.GetShaderInfoLog(VertexShader)
                Console.WriteLine(InfoLog)
            End If

            GL.CompileShader(FragmentShader)
            GL.GetShader(FragmentShader, ShaderParameter.CompileStatus, Success)
            If Success = 0 then
                Dim InfoLog as Integer = GL.GetShaderInfoLog(FragmentShader)
                Console.WriteLine(InfoLog)
            End If

            Me.Handle = GL.CreateProgram()

            GL.AttachShader(Handle, VertexShader)
            GL.AttachShader(Handle, FragmentShader)

            GL.LinkProgram(Handle)

            GL.GetProgram(Handle, GetProgramParameterName.LinkStatus, Success)
            if Success = 0 then
                Dim InfoLog as String = GL.GetProgramInfoLog(Handle)
                Console.WriteLine(InfoLog)
            End If

            GL.DetachShader(Me.Handle, VertexShader)
            GL.DetachShader(Me.Handle, FragmentShader)
            GL.DeleteShader(FragmentShader)
            GL.DeleteShader(VertexShader)

            Me.ColorUniform = GL.GetUniformLocation(Me.Handle, "uColor")

            Me.MatrixUniform = GL.GetUniformLocation(Me.Handle, "uMatrix")

            Console.WriteLine("program done")

        End Sub
        '-----------------------------------------------------------------------------

        '=============================================================================
        '
        '=============================================================================
        Public Sub Use()

            GL.UseProgram(Handle)

        End Sub
        '-----------------------------------------------------------------------------

        '=============================================================================
        '
        '=============================================================================
        Public Sub SetColor(R as Single, G as Single, B as Single)

            'Console.WriteLine(String.Format("color uniform is {0} ", Me.ColorUniform))
            GL.Uniform4(Me.ColorUniform, R, G, B, 1.0F)

        End Sub
        '-----------------------------------------------------------------------------

        '=============================================================================
        '
        '=============================================================================
        Public Sub SetTrasform(Sc as Single, Ry as Single, Rz as Single)

            Dim M_Rz As Matrix4 = Matrix4.CreateRotationZ(MathHelper.DegreesToRadians(Rz))
            Dim M_Ry As Matrix4 = Matrix4.CreateRotationY(MathHelper.DegreesToRadians(Ry))
            Dim M_Sc As Matrix4 = Matrix4.CreateScale(Sc, Sc, Sc)

            GL.UniformMatrix4(Me.MatrixUniform, True, M_Rz * M_Ry * M_Sc)

        End Sub
        '-----------------------------------------------------------------------------

        Private DisposedValue As Boolean = False
        '=============================================================================
        '
        '=============================================================================
        Private Protected Sub Dispose(Disposing as Boolean)

            If (Not DisposedValue) Then

                GL.DeleteProgram(Handle)

                DisposedValue = True

            End If

        End Sub
        '-----------------------------------------------------------------------------


        '=============================================================================
        '
        '=============================================================================
        Sub Dispose() Implements IDisposable.Dispose

            Dispose(True)
            GC.SuppressFinalize(Me)

            Console.WriteLine("shader program disposed")

        End Sub
        '-----------------------------------------------------------------------------

    End Class
    '------------------------------------------------------------------------------------------




    '//////////////////////////////////////////////////////////////////////////////////////////////
    ''' The visual representation of a surface
    '//////////////////////////////////////////////////////////////////////////////////////////////
    Public Class VisualElement

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private VertexBufferId as Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private VertexArrayObjectId as Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private LatticeBufferId as Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        '
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private ParentSurface as Surface

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The size of the vertex buffer
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private V as Integer = 0

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The size of the elements buffer
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private E as Integer = 0

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The size of the elements buffer
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private Symmetric As Boolean = False

        '==========================================================================================
        '
        '==========================================================================================
        Public Sub LoadSurface (Item as Surface, Symmetric As Boolean)

            Me.ParentSurface = Item
            Me.Symmetric = Symmetric
            Me.LoadSurface()

        End Sub
        '------------------------------------------------------------------------------------------


        '==========================================================================================
        '
        '==========================================================================================
        Public Sub LoadSurface()

            If Me.ParentSurface Is Nothing Then
                Exit Sub
            End If

            'Vertices
            '-----------------------------

            if Me.VertexBufferId = 0 Then
                Me.VertexBufferId = GL.GenBuffer()
            End If

            if Me.VertexArrayObjectId = 0 Then
                Me.VertexArrayObjectId = GL.GenVertexArray()
            End If

            Dim Vertices(3 * Me.ParentSurface.Mesh.Nodes.Count) As Single
            Dim i as Integer = 0

            For Each Node In Me.ParentSurface.Mesh.Nodes

                Vertices(i) = Node.Position.X
                i += 1
                if Me.Symmetric then
                    Vertices(i) = -Node.Position.Y
                Else
                    Vertices(i) =  Node.Position.Y
                End If
                i += 1
                Vertices(i) = Node.Position.Z
                i += 1

            Next

            Me.V = i

            GL.BindVertexArray(Me.VertexArrayObjectId)
            GL.BindBuffer(BufferTarget.ArrayBuffer, Me.VertexBufferId)
            GL.BufferData(BufferTarget.ArrayBuffer, Me.V * Len(New Single), Vertices, BufferUsageHint.StaticDraw)
            GL.VertexAttribPointer(0, 3, VertexAttribPointerType.Float, False, 3 * Len(New Single), 0)
            GL.EnableVertexAttribArray(0)

            ' Lattice
            '-----------------------------

            if Me.LatticeBufferId = 0 Then
                Me.LatticeBufferId = GL.GenBuffer()
            End If

            Dim Lattice(2 * Me.ParentSurface.Mesh.Lattice.Count) As UInteger
            i = 0

            For Each Segment In Me.ParentSurface.Mesh.Lattice

                Lattice(i) = Segment.N1
                i += 1
                Lattice(i) = Segment.N2
                i += 1

            Next

            Me.E = i

            GL.BindBuffer(BufferTarget.ElementArrayBuffer, Me.LatticeBufferId)
            GL.BufferData(BufferTarget.ElementArrayBuffer, Me.E * Len(New UInteger), Lattice, BufferUsageHint.StaticDraw)

            System.Console.WriteLine(String.Format("loaded {0} vertices and {1} elements for '{2}'", Me.V, Me.E, Me.ParentSurface.Name))

        End Sub
        '------------------------------------------------------------------------------------------


        '==========================================================================================
        ' Clears the visual element
        '==========================================================================================
        Public Sub Clear()

            GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
            GL.DeleteBuffer(Me.VertexBufferId)
            Me.VertexBufferId = 0

            GL.BindBuffer(BufferTarget.ElementArrayBuffer, 0)
            GL.DeleteBuffer(Me.LatticeBufferId)
            Me.LatticeBufferId = 0

        End Sub
        '------------------------------------------------------------------------------------------


        '==========================================================================================
        ' Draws the visual element
        '==========================================================================================
        Public Sub RenderFrame(Shader as ShaderProgram)

            If Me.E > 0 And Me.V > 0 then

                Shader.Use()
                Shader.SetColor(Me.ParentSurface.VisualProperties.ColorSurface.R, _
                                Me.ParentSurface.VisualProperties.ColorSurface.G, _
                                Me.ParentSurface.VisualProperties.ColorSurface.B)
                'Shader.SetTrasform(0.1#, 45.0#, 25.0#)

                GL.BindVertexArray(Me.VertexArrayObjectId)
                GL.BindBuffer (BufferTarget.ElementArrayBuffer, Me.LatticeBufferId)
                GL.DrawElements(PrimitiveType.Lines, Me.E, DrawElementsType.UnsignedInt, 0)

            End If

        End Sub
        '------------------------------------------------------------------------------------------


    End Class
    '----------------------------------------------------------------------------------------------


    
    '//////////////////////////////////////////////////////////////////////////////////////////////
    ''' The visual representation of the model
    '//////////////////////////////////////////////////////////////////////////////////////////////
    Public Class VisualElements

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The collection of visual elements for the design model
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private Elements As New List(Of VisualElement)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The shader program with which all surfaces are drawn
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private Shader as ShaderProgram

        '==========================================================================================
        ' Reloads the design model
        '==========================================================================================
        Public Sub LoadResources()

            Shader = new ShaderProgram("shader.vert", "shader.frag")

        End Sub
        '------------------------------------------------------------------------------------------


        '==========================================================================================
        ' Sets the transformation
        '==========================================================================================
        Public Sub SetTrasform(Sc as Single, Ry as Single, Rz as Single)

            Shader.Use()
            Shader.SetTrasform(Sc, Ry, Rz)

        End Sub
        '------------------------------------------------------------------------------------------


        '==========================================================================================
        ' Reloads the design model
        '==========================================================================================
        Public Sub ClearResources()

            Shader.Dispose()

        End Sub
        '------------------------------------------------------------------------------------------


        '==========================================================================================
        ' Reloads the design model
        '==========================================================================================  
        Public Sub LoadDesignModel()
            
            ' Remove all resources first
            '-----------------------------

            For Each Element In Elements
                Element.Clear()
            Next

            Elements.Clear()

            ' Reload surfaces
            '-----------------------------
            
            For Each Surface In DataStore.ProjectRoot.Model.Objects

                Dim Element as new VisualElement
                Element.LoadSurface (Surface, False)
                Elements.Add(Element)

                ' Load symmetric side of wings
                '-----------------------------
                If TypeOf Surface Is LiftingSurface Then
                    Dim Wing As LiftingSurface = Surface
                    If Wing.Symmetric Then
                        Dim OtherSide as new VisualElement
                        OtherSide.LoadSurface (Wing, True)
                        Elements.Add(OtherSide)
                    End If
                End If

            Next

        End Sub
        '------------------------------------------------------------------------------------------




        '==========================================================================================
        ' Use this to refresh the visual model when the design model is adapted but
        ' keeps the same surfaces
        '==========================================================================================  
        Public Sub RefreshDesignModel()
            
            For Each Element In Elements
                Element.LoadSurface()
            Next

        End Sub
        '------------------------------------------------------------------------------------------




        '==========================================================================================
        ' Render the full model
        '==========================================================================================
        Public Sub RenderFrame()

            For Each Element In Elements

                Element.RenderFrame(Shader)

            Next

        End Sub
        '------------------------------------------------------------------------------------------

    End Class
    '-----------------------------------------------------------------------------


    
End Module
'-----------------------------------------------------------------------------

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
Imports VGL.AeroTools.Settings
Imports VGL.DesignTools.Interface
Imports VGL.MathTools.Algebra.CustomMatrices
Imports VGL.MathTools.Algebra.EuclideanSpace

'#############################################################################
' Unit: Surface
'
' This unit declares a generic surface, which is the base of all models
'#############################################################################
Namespace DesignTools.Models.Components.Basics

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Basic definition of a model surface. 
    ' All model surfaces in the library must inherit from this class.
    '//////////////////////////////////////////////////////////////////////////////////////////
    ' <remarks></remarks>
    Public MustInherit Class Surface

        Implements IOperational

        Implements ISelectable

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Surface identifier.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Id As Guid

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Surface name.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Name As String

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The inertial properties of this model.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Inertia As InertialProperties

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Mesh.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Mesh As Mesh

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Number of segments in the mesh.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property NumberOfSegments As Integer
            Get
                Return Mesh.Lattice.Count
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Number of nodes in the mesh.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property NumberOfNodes As Integer
            Get
                If Not IsNothing(Mesh.Nodes) Then
                    Return Mesh.Nodes.Count
                Else
                    Return 0
                End If
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Number of panels in the mesh.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property NumberOfPanels As Integer
            Get
                If Not IsNothing(Mesh.Panels) Then
                    Return Mesh.Panels.Count
                Else
                    Return 0
                End If
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Surface visual properties.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property VisualProperties As VisualProperties

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if the surface participates in the calculation model.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property IncludeInCalculation As Boolean = False

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicate if the GUI has to block the content of this surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property LockContent As Boolean = True

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Position of the surface in the global coordinates system.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Position As New Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Center of rotation of the surface in the local coordinates system.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property CenterOfRotation As New Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Orientation of the surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Orientation As New OrientationAngles

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Scale of this surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property SizeScale As Double = 1.0

#Region " Operations "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Local directions.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property LocalDirections As New Base3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Moves the origin of the local coordinates to a given point.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overridable Sub MoveTo(ByVal Vector As Vector3) Implements IOperational.MoveTo

            Position.X = Vector.X
            Position.Y = Vector.Y
            Position.Z = Vector.Z

            GenerateMesh()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Changes the orientation of the surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overridable Sub Orientate(ByVal Point As Vector3, ByVal Ori As OrientationAngles) Implements IOperational.Orientate

            Orientation.Angle1 = Ori.Angle1
            Orientation.Angle2 = Ori.Angle2
            Orientation.Angle3 = Ori.Angle3

            CenterOfRotation.X = Point.X
            CenterOfRotation.Y = Point.Y
            CenterOfRotation.Z = Point.Z

            GenerateMesh()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Scales the coordinates.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overridable Sub Scale(ByVal Scale As Double) Implements IOperational.Scale

            SizeScale = Scale

            GenerateMesh()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Align the surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overridable Sub Align(ByVal P1 As Vector3, ByVal P2 As Vector3, ByVal P3 As Vector3, ByVal P4 As Vector3) Implements IOperational.Align

            Mesh.Align()

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if the surface is currently selected.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Active As Boolean = False Implements ISelectable.Active

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Unselects all nodal points.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub UnselectAll() Implements ISelectable.UnselectAll

            For Each Node In Mesh.Nodes
                Node.Active = False
            Next

            For Each Panel In Mesh.Panels
                Panel.Active = False
            Next

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Clones the surface
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public MustOverride Function Clone() As Surface

#Region " Meshing "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates the mesh.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overridable Sub GenerateMesh()

            RaiseEvent MeshUpdated()

        End Sub
        '------------------------------------------------------------------------------------------

        Public Event MeshUpdated()

#End Region

#Region " IO "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Converts the boolean to a character '*' or '-'
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Function GetCharFlag(Flag As Boolean) As Char

            If Flag Then
                Return "*"
            Else
                Return "-"
            End If

        End Function

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes the surface to an XML node
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public MustOverride Sub WriteToXML(ByRef writes As XmlWriter)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads the surface from an XML node
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public MustOverride Sub ReadFromXML(ByRef reader As XmlReader)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes an ASCII STL file containing the model mesh.
        ' Note that this method can be overriden by classes inheriting Surface, so this is probably not the final implementation.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overridable Sub ExportStlFile(FilePath As String, Optional Append As Boolean = False, Optional Transformation As Matrix3x3 = Nothing)

            Dim FileId As Integer = FreeFile()

            If Append Then
                FileOpen(FileId, FilePath, OpenMode.Append)
            Else
                FileOpen(FileId, FilePath, OpenMode.Output)
            End If

            Try

                If Not Append Then
                    PrintLine(FileId, String.Format("solid {0}", Name))
                End If

                For Each Panel In Mesh.Panels

                    If Panel.IsTriangular Then

                        Dim V1 As New Vector3(Mesh.Nodes(Panel.N1).Position)
                        Dim V2 As New Vector3(Mesh.Nodes(Panel.N2).Position)
                        Dim V3 As New Vector3(Mesh.Nodes(Panel.N3).Position)

                        If Transformation IsNot Nothing Then
                            V1.Transform(Transformation)
                            V2.Transform(Transformation)
                            V3.Transform(Transformation)
                        End If

                        Dim N As Vector3 = (V3 - V1) ^ (V2 - V1)
                        N.Normalize()

                        PrintLine(FileId, String.Format("facet normal {0:E3} {1:E3} {2:E3}", N.X, N.Y, N.Z))
                        PrintLine(FileId, "   outer loop")
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V1.X, V1.Y, V1.Z))
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V2.X, V2.Y, V2.Z))
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V3.X, V3.Y, V3.Z))
                        PrintLine(FileId, "   endloop")
                        PrintLine(FileId, "endfacet")

                    Else

                        Dim V1 As New Vector3(Mesh.Nodes(Panel.N1).Position)
                        Dim V2 As New Vector3(Mesh.Nodes(Panel.N2).Position)
                        Dim V3 As New Vector3(Mesh.Nodes(Panel.N3).Position)
                        Dim V4 As New Vector3(Mesh.Nodes(Panel.N4).Position)

                        If Transformation IsNot Nothing Then
                            V1.Transform(Transformation)
                            V2.Transform(Transformation)
                            V3.Transform(Transformation)
                            V4.Transform(Transformation)
                        End If

                        Dim N As Vector3 = (V3 - V1) ^ (V2 - V1)
                        N.Normalize()

                        PrintLine(FileId, String.Format("facet normal {0:E3} {1:E3} {2:E3}", N.X, N.Y, N.Z))
                        PrintLine(FileId, "   outer loop")
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V1.X, V1.Y, V1.Z))
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V2.X, V2.Y, V2.Z))
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V3.X, V3.Y, V3.Z))
                        PrintLine(FileId, "   endloop")
                        PrintLine(FileId, "endfacet")

                        N = (V1 - V3) ^ (V4 - V3)
                        N.Normalize()

                        PrintLine(FileId, String.Format("facet normal {0:E3} {1:E3} {2:E3}", N.X, N.Y, N.Z))
                        PrintLine(FileId, "   outer loop")
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V3.X, V3.Y, V3.Z))
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V4.X, V4.Y, V4.Z))
                        PrintLine(FileId, String.Format("      vertex {0:E3} {1:E3} {2:E3}", V1.X, V1.Y, V1.Z))
                        PrintLine(FileId, "   endloop")
                        PrintLine(FileId, "endfacet")

                    End If

                Next

                If Not Append Then
                    PrintLine(FileId, String.Format("endsolid {0}", Name))
                End If

            Finally

                FileClose(FileId)

            End Try

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes a connectivity file containing the model mesh in native VGL format.
        ' Note that this method can be overriden by classes inheriting Surface, so this is probably not the final implementation.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overridable Sub ExportNativeFile(FilePath As String, Optional Append As Boolean = False, Optional Transformation As Matrix3x3 = Nothing)

            Dim FileId As Integer = FreeFile()

            If Append Then
                FileOpen(FileId, FilePath, OpenMode.Append)
            Else
                FileOpen(FileId, FilePath, OpenMode.Output)
            End If

            Try

                If Not Append Then
                    PrintLine(FileId, String.Format("SURFACE {0}", Name))
                End If

                ' Nodes

                PrintLine(FileId, "NODES")

                For Each Node In Mesh.Nodes

                    Dim V As New Vector3(Node.Position)

                    If Transformation IsNot Nothing Then
                        V.Transform(Transformation)
                    End If

                    PrintLine(FileId, String.Format("{0:E14} {1:E14} {2:E14}", V.X, V.Y, V.Z))

                Next

                ' Panels

                Dim First As Boolean = True

                For Each Panel In Mesh.Panels

                    If First Then

                        PrintLine(FileId, "PANELS")

                        First = False

                    End If

                    If Panel.IsTriangular Then

                        PrintLine(FileId, String.Format("{0}{1}{2} {3:D} {4:D} {5:D}",
                                                        GetCharFlag(Panel.IsSlender),
                                                        GetCharFlag(Panel.IsReversed),
                                                        GetCharFlag(Panel.IsPrimitive),
                                                        Panel.N1,
                                                        Panel.N2,
                                                        Panel.N3))

                    Else

                        PrintLine(FileId, String.Format("{0}{1}{2} {3:D} {4:D} {5:D} {6:D}",
                                                        GetCharFlag(Panel.IsSlender),
                                                        GetCharFlag(Panel.IsReversed),
                                                        GetCharFlag(Panel.IsPrimitive),
                                                        Panel.N1,
                                                        Panel.N2,
                                                        Panel.N3,
                                                        Panel.N4))


                    End If

                Next

            Finally

                FileClose(FileId)

            End Try

        End Sub
        '------------------------------------------------------------------------------------------


        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes a the surface mesh in a format that can be directly ploted in Scilab using plot3d2 function
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ExportScilabFile(FilePath As String, Optional Append As Boolean = False, Optional Transformation As Matrix3x3 = Nothing)

            Dim FileId As Integer = FreeFile()

            If Append Then
                FileOpen(FileId, FilePath, OpenMode.Append)
            Else
                FileOpen(FileId, FilePath, OpenMode.Output)
            End If

            Try

                If Not Append Then

                    PrintLine(FileId, String.Format("SURFACE {0}", Name))

                End If

                ' Nodes

                PrintLine(FileId, "NODES")

                For Each Node In Mesh.Nodes

                    Dim V As New Vector3(Node.Position)

                    If Transformation IsNot Nothing Then
                        V.Transform(Transformation)
                    End If

                    PrintLine(FileId, String.Format("{0:E14} {1:E14} {2:E14}", V.X, V.Y, V.Z))

                Next

                ' Panels

                Dim First As Boolean = True

                For Each Panel In Mesh.Panels

                    If First Then

                        PrintLine(FileId, "PANELS")

                        First = False

                    End If

                    If Panel.IsTriangular Then

                        PrintLine(FileId, String.Format("{0}{1}{2} {3:D} {4:D} {5:D}",
                                                        GetCharFlag(Panel.IsSlender),
                                                        GetCharFlag(Panel.IsReversed),
                                                        GetCharFlag(Panel.IsPrimitive),
                                                        Panel.N1,
                                                        Panel.N2,
                                                        Panel.N3))

                    Else

                        PrintLine(FileId, String.Format("{0}{1}{2} {3:D} {4:D} {5:D} {6:D}",
                                                        GetCharFlag(Panel.IsSlender),
                                                        GetCharFlag(Panel.IsReversed),
                                                        GetCharFlag(Panel.IsPrimitive),
                                                        Panel.N1,
                                                        Panel.N2,
                                                        Panel.N3,
                                                        Panel.N4))


                    End If

                Next

            Finally

                FileClose(FileId)

            End Try

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class

End Namespace

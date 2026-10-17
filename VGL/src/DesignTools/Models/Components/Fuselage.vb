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
Imports DotNumerics.Interpolation
Imports VGL.AeroTools.Settings
Imports VGL.AeroTools.IoHelper
Imports VGL.DesignTools.Models.Components.Basics
Imports VGL.DesignTools.Interface
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports VGL.MathTools.Algebra.CustomMatrices

'#############################################################################
' Unit: ImportedSurface
'
' This unit provides a fuselage model composed of a surface extruded along
' generic cross sections. The model can be automatically linked to lifting 
' surface by so-called "anchors".
'#############################################################################
Namespace DesignTools.Models.Components

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Contains information about a parent wing.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class WingAnchorInfo

        Public Property ParentID As Guid
        Public Property AnchorFromTip = False
        Public Property AnchorFromRoot As Boolean = False

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a point in the surface where a vertex has to be added.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class AnchorPoint

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Curvilinear coordinate.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property s As Double

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Longitudinal coordinate.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Z As Double

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Perimeter of the section associated to this anchor point used to find back the
        ' cartesian coordinates.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Perimeter As Double

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a series of segments that have to be projected on a extruded surface.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class AnchorLine

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Lines of extrusion.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Lines As New List(Of Line3)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Surface coordinates anchor points.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Projections As New List(Of AnchorPoint)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates the initial position in the mesh where the points were located.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property BeginIndex As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates the end position in the mesh where the points were located.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property EndIndex As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Gather information about the parent lifting surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property WingAnchorInfo As WingAnchorInfo

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Inidcates if the projections where created.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property Generated As Boolean
            Get
                Return Projections.Count > 1
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Moves all points the given displacements.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub TranslatePoints(ByVal dX As Double, ByVal dY As Double, ByVal dZ As Double)

            For Each Line In Lines

                Line.Point.X += dX
                Line.Point.Y += dY
                Line.Point.Z += dZ

            Next

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Creates a new instance with values read from an xml file.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New(ByVal Stream As XmlReader)
            ReadFromXML(Stream)
        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Creates a new instance.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New()

        End Sub
        '------------------------------------------------------------------------------------------

#Region "IO"

        Public Sub ReadFromXML(ByRef Stream As XmlReader)

            While Stream.Read

                If Stream.IsStartElement Then

                    Select Case Stream.Name

                        Case "Line"

                            Dim Line As New Line3

                            Line.Point.X = IOXML.ReadDouble(Stream, "Xp", 0.0)
                            Line.Point.Y = IOXML.ReadDouble(Stream, "Yp", 0.0)
                            Line.Point.Z = IOXML.ReadDouble(Stream, "Zp", 0.0)

                            Line.Direction.X = IOXML.ReadDouble(Stream, "Xd", 0.0)
                            Line.Direction.Y = IOXML.ReadDouble(Stream, "Yd", 0.0)
                            Line.Direction.Z = IOXML.ReadDouble(Stream, "Zd", 0.0)

                            Lines.Add(Line)

                        Case "Info"

                            WingAnchorInfo = New WingAnchorInfo()

                            WingAnchorInfo.ParentID = New Guid(IOXML.ReadString(Stream, "ParentID", Guid.NewGuid.ToString))
                            WingAnchorInfo.AnchorFromRoot = IOXML.ReadBoolean(Stream, "Root", False)
                            WingAnchorInfo.AnchorFromTip = IOXML.ReadBoolean(Stream, "Tip", False)

                    End Select

                End If

            End While

        End Sub
        '------------------------------------------------------------------------------------------

        Public Sub WriteToXML(ByRef Stream As XmlWriter)

            For Each Line In Lines

                Stream.WriteStartElement("Line")

                Stream.WriteAttributeString("Xp", Line.Point.X.ToString)
                Stream.WriteAttributeString("Yp", Line.Point.Y.ToString)
                Stream.WriteAttributeString("Zp", Line.Point.Z.ToString)

                Stream.WriteAttributeString("Xd", Line.Direction.X.ToString)
                Stream.WriteAttributeString("Yd", Line.Direction.Y.ToString)
                Stream.WriteAttributeString("Zd", Line.Direction.Z.ToString)
                Stream.WriteEndElement()

            Next

            If Not IsNothing(WingAnchorInfo) Then

                Stream.WriteStartElement("Info")

                Stream.WriteAttributeString("ParentID", WingAnchorInfo.ParentID.ToString)
                Stream.WriteAttributeString("Root", WingAnchorInfo.AnchorFromRoot)
                Stream.WriteAttributeString("Tip", WingAnchorInfo.AnchorFromTip)
                Stream.WriteEndElement()

            End If

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The grid of a mesh feature
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class FeatureGrid

        Public e As Integer

        Public nns As Integer

        Public nnz As Integer

        Public zo As Double

        Public zf As Double

        Public Nodes As New List(Of Vector2)

        Public ParentAnchor As AnchorLine

        Public AnchorIndices() As Integer

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a planar cross section.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class CrossSection

        Implements IComparable

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Te position of the cross section along the Z axis.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Z As Double = 0.0#

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The nodes describing the section.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Vertices As New List(Of Vector2)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if this section breaks the spline first order continuity.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property BrokenEdge As Boolean = False

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The total perimeter.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _Perimeter As Double

        Public Sub New()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Creates a new section from an XML node.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New(ByRef Stream As XmlReader)

            ReadFromXML(Stream)

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Calculates and caches the perimeter.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub CalculatePerimeter()

            _Perimeter = 0

            For i = 0 To Vertices.Count - 2

                _Perimeter += Vertices(i).DistanceTo(Vertices(i + 1))

            Next

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Perimeter of the section.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property Perimeter As Double
            Get
                Return _Perimeter
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Returns an interpolated point at the given line coordinate s
        ' (s is a curvilinear dimensionless coordinate (from 0 to 1)
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Function GetPoint(ByVal s As Double) As Vector2

            If Vertices.Count > 1 Then

                Dim s_i As Double = 0
                Dim s_f As Double = 0
                Dim i_i As Integer = -1

                If s < 0 Then s = 0
                If s > 1 Then s = 1

                For i = 0 To Vertices.Count - 2

                    s_i = s_f
                    s_f += Vertices(i).DistanceTo(Vertices(i + 1))

                    If s_f / Perimeter >= s Then
                        i_i = i
                        Exit For
                    End If

                Next

                ' If it has not been assigned:

                If i_i = -1 Then

                    i_i = Vertices.Count - 2

                End If

                s_f /= Perimeter
                s_i /= Perimeter

                Dim f As Double = (s_f - s) / (s_f - s_i) ' 0 if s = s_f; 1 if s = s_o

                Dim point As Vector2 = New Vector2()

                point.X = f * Vertices(i_i).X + (1 - f) * Vertices(i_i + 1).X
                point.Y = f * Vertices(i_i).Y + (1 - f) * Vertices(i_i + 1).Y

                Return point

            ElseIf Vertices.Count = 1 Then

                Dim point As Vector2 = New Vector2()

                point.X = Vertices(0).X
                point.Y = Vertices(0).Y

                Return point

            Else

                Return Nothing

            End If

        End Function

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Compares the position of this cross section with another section.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Function CompareTo(obj As Object) As Integer Implements IComparable.CompareTo

            Dim OtherSection As CrossSection = obj

            If OtherSection.Z > Z Then
                Return -1
            ElseIf OtherSection.Z = Z
                Return 0
            Else
                Return 1
            End If

        End Function

#Region " IO "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes the section to an XML file.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub WriteToXML(ByRef Stream As XmlWriter)

            Stream.WriteAttributeString("Z", String.Format("{0}", Z))
            Stream.WriteAttributeString("BrokenEdge", String.Format("{0}", BrokenEdge))

            For Each Vertex In Vertices

                Stream.WriteStartElement("Vertex")
                Stream.WriteAttributeString("X", String.Format("{0}", Vertex.X))
                Stream.WriteAttributeString("Y", String.Format("{0}", Vertex.Y))
                Stream.WriteEndElement()

            Next

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads the wing from an XML file.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ReadFromXML(ByRef Stream As XmlReader)

            Vertices.Clear()

            Z = IOXML.ReadDouble(Stream, "Z", 0.0)
            BrokenEdge = IOXML.ReadBoolean(Stream, "BrokenEdge", True)

            Dim subtree As XmlReader = Stream.ReadSubtree()

            While subtree.ReadToFollowing("Vertex")

                Dim Vertex As New Vector2
                Vertex.X = IOXML.ReadDouble(Stream, "X", 0.0)
                Vertex.Y = IOXML.ReadDouble(Stream, "Y", 0.0)
                Vertices.Add(Vertex)

            End While

            CalculatePerimeter()

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' All types of meshes.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum MeshTypes

        UnstructuredTriangles = 0
        StructuredQuadrilaterals = 1

    End Enum

    Public Enum InterpolationTypes

        Linear = 0
        CubicSpline = 1

    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a surface defined by parallel planar cross sections.
    ' Fuselages can be attached to one or more lifting surface, as long as these do
    ' not supperpose in the direction of the longitudinal axis.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class Fuselage

        Inherits Surface

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a new surface using default initial values.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub New()

            Mesh = New Mesh()

            Id = Guid.NewGuid
            Name = "Fuselage"
            CrossSections = New List(Of CrossSection)
            VisualProperties = New VisualProperties(ComponentTypes.etFuselage)
            VisualProperties.ThicknessMesh = 1.0
            VisualProperties.ShowSurface = True
            VisualProperties.ShowMesh = True
            IncludeInCalculation = True
            AnchorLines = New List(Of AnchorLine)
            CrossRefinement = 10

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' List containing cross sections used to extrude the surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property CrossSections As List(Of CrossSection)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Gathers all ankor lines.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property AnchorLines As List(Of AnchorLine)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Defines the type of mesh
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property MeshType As MeshTypes = MeshTypes.StructuredQuadrilaterals

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The interpolation used between the declared cross sections.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property InterpolationType As InterpolationTypes = InterpolationTypes.CubicSpline

        Private _LongitudinalRefinement As Integer = 2

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates the  number of paneles in the transverse direction.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property LongitudinalRefinement As Integer
            Set(value As Integer)
                If value >= 2 Then _LongitudinalRefinement = value
            End Set
            Get
                Return _LongitudinalRefinement
            End Get
        End Property

        Private _CrossRefinement As Integer = 2

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates the  number of paneles in the transverse direction.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property CrossRefinement As Integer
            Set(value As Integer)
                If value >= 2 Then _CrossRefinement = value
            End Set
            Get
                Return _CrossRefinement
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' List containing cross sections used to extrude the surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _CrossSectionsToDisplay(0)() As Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Gets a point on the surface given its 2D surface coordinates.
        ' Z is the longitudinal coordinate (along extrusion axis).
        ' S is the transverse dimensionless coordinate.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Function GetPoint(ByVal Z As Double, ByVal S As Double) As Vector3

            ' Limit Z (locally) to the cross section longitudinal range
            '----------------------------------------------------------

            If Z < CrossSections(0).Z Then
                Z = CrossSections(0).Z
            ElseIf Z > CrossSections(CrossSections.Count - 1).Z
                Z = CrossSections(CrossSections.Count - 1).Z
            End If

            Select Case InterpolationType

                Case InterpolationTypes.Linear

                    ' Linear interpolation between two sections
                    '------------------------------------------------

                    Dim i_a As Integer = 0
                    Dim i_b As Integer = 1

                    For i = 1 To CrossSections.Count - 1
                        If CrossSections(i).Z >= Z Then
                            i_a = i - 1
                            i_b = i
                            Exit For
                        End If
                    Next

                    Dim point_b = CrossSections(i_b).GetPoint(S)
                    Dim point_a = CrossSections(i_a).GetPoint(S)

                    Dim f As Double = (CrossSections(i_b).Z - Z) / (CrossSections(i_b).Z - CrossSections(i_a).Z)

                    Return New Vector3((1 - f) * point_b.X + f * point_a.X, (1 - f) * point_b.Y + f * point_a.Y, Z)

                Case InterpolationTypes.CubicSpline

                    Dim n As Integer = CrossSections.Count - 1

                    ' Find the previous and next broken edges (if any)
                    '------------------------------------------------

                    Dim i_a As Integer = 0
                    Dim i_b As Integer = n

                    For i = 0 To CrossSections.Count - 1
                        If CrossSections(i).BrokenEdge AndAlso CrossSections(i).Z <= Z Then
                            i_a = i
                        End If
                        If CrossSections(i).BrokenEdge AndAlso CrossSections(i).Z >= Z Then
                            i_b = i
                            Exit For
                        End If
                    Next

                    Dim Count As Integer = i_b - i_a

                    If Count = 0 Then

                        ' The Z coordinate is exactly in a cross section
                        '------------------------------------------------

                        Dim Point = CrossSections(i_b).GetPoint(S)

                        Return New Vector3(Point.X, Point.Y, Z)

                    Else

                        ' Spline between two or more sections
                        '------------------------------------------------

                        Dim U(Count) As Double
                        Dim X(Count) As Double
                        Dim Y(Count) As Double

                        For i = 0 To Count
                            U(i) = CrossSections(i_a + i).Z
                            Dim Point = CrossSections(i_a + i).GetPoint(S)
                            X(i) = Point.X
                            Y(i) = Point.Y
                        Next

                        Dim SplineX As CubicSpline = CubicSpline.InterpolateNaturalSorted(U, X)
                        Dim SplineY As CubicSpline = CubicSpline.InterpolateNaturalSorted(U, Y)

                        Return New Vector3(SplineX.Interpolate(Z), SplineY.Interpolate(Z), Z)

                    End If

            End Select

            Return New Vector3

        End Function

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a triangular or quadrilateral mesh.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overrides Sub GenerateMesh()

            Select Case MeshType
                Case MeshTypes.UnstructuredTriangles
                    GenerateTriangularMesh()
                Case MeshTypes.StructuredQuadrilaterals
                    GenerateQuadrilateralMesh()
            End Select

            ' Launch base sub to raise update event.

            MyBase.GenerateMesh()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Creates a mesh of triangular panels (not implemented).
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub GenerateTriangularMesh()

            Throw New NotImplementedException

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a structured mesh of quadrilaterals using the provided cross sections and wing anchors.
        ' Visit the wikibook pages for informaton about how fuselages are generated.
        ' Note that this cannot handle anchors supperposed is the Z direction.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub GenerateQuadrilateralMesh()

            Mesh.Panels.Clear()
            Mesh.Nodes.Clear()

            Try

                UpdateAnchors()

                ' First check: get valid anchors

                Dim ValidAnchors As New List(Of AnchorLine)

                For Each ank In AnchorLines

                    If ank.Projections.Count > 1 Then

                        Dim valid As Boolean = True

                        For i = 1 To ank.Projections.Count - 1

                            If ank.Projections(i).Z <= ank.Projections(i - 1).Z Then

                                valid = False

                            End If

                            If ank.Projections(i - 1).Z < CrossSections(0).Z Or ank.Projections(i).Z > CrossSections(CrossSections.Count - 1).Z Then

                                valid = False

                            End If

                        Next

                        If valid Then

                            ValidAnchors.Add(ank)

                        End If

                    End If

                Next

                ' Second check: remove supperposed anchors

                Dim Supperposed As Boolean = True

                While Supperposed

                    Supperposed = False

                    For i = 0 To ValidAnchors.Count - 1

                        For j = 0 To ValidAnchors.Count - 1

                            If j <> i Then

                                Dim Zi0 As Integer = ValidAnchors(i).Projections(0).Z
                                Dim Zin As Integer = ValidAnchors(i).Projections(ValidAnchors(i).Projections.Count - 1).Z

                                Dim Zj0 As Integer = ValidAnchors(j).Projections(0).Z
                                Dim Zjn As Integer = ValidAnchors(j).Projections(ValidAnchors(j).Projections.Count - 1).Z

                                If (Zi0 >= Zj0 And Zi0 <= Zjn) Or (Zin >= Zj0 And Zin <= Zjn) Then
                                    Supperposed = True
                                    ValidAnchors.RemoveAt(j)
                                    Exit For
                                End If

                            End If

                        Next

                        If Supperposed Then Exit For

                    Next

                End While

                ' Generate grids:

                Dim npz As Integer = LongitudinalRefinement
                Dim nps As Integer = CrossRefinement ' number of panels in s direction
                Dim l As Double = CrossSections(CrossSections.Count - 1).Z - CrossSections(0).Z
                Dim addTrailingWake As Boolean = False

                Dim Grids As New List(Of FeatureGrid)

                For Each ValidAnchor In ValidAnchors

                    Dim Grid As New FeatureGrid

                    Dim e As Integer
                    Dim s_mean As Double = 0.0#

                    For Each point In ValidAnchor.Projections

                        s_mean += point.s / point.Perimeter

                    Next

                    s_mean /= ValidAnchor.Projections.Count

                    e = Math.Round(s_mean * nps)

                    If e < 0 Then e = 0

                    If e > nps Then e = nps - 1

                    Grid.e = e
                    Grid.nns = nps + 1
                    Grid.nnz = ValidAnchor.Projections.Count
                    Grid.zo = ValidAnchor.Projections(0).Z
                    Grid.zf = ValidAnchor.Projections(ValidAnchor.Projections.Count - 1).Z
                    Grid.ParentAnchor = ValidAnchor
                    ReDim Grid.AnchorIndices(Grid.nnz)

                    For i = 0 To ValidAnchor.Projections.Count - 1

                        Dim z As Double = ValidAnchor.Projections(i).Z

                        For j = 0 To nps

                            Dim s As Double = 0.0#
                            Dim ps As Double = ValidAnchor.Projections(i).s / ValidAnchor.Projections(i).Perimeter

                            If j <= e Then
                                s = ps * j / e
                            Else
                                s = ps + (1 - ps) * (j - e) / (nps - e)
                            End If

                            If j = 0 Then s = 0

                            If j = nps Then s = 1

                            Grid.Nodes.Add(New Vector2(z, s))

                        Next

                    Next

                    Grids.Add(Grid)

                Next

                ' Generate mesh progressively:

                If Grids.Count = 0 Then

                    For i = 0 To npz

                        Dim z As Double = CrossSections(0).Z + l * i / npz

                        For j = 0 To nps

                            Dim s As Double = j / nps

                            Mesh.Nodes.Add(New NodalPoint(GetPoint(z, s)))

                            If (i > 0) And (j > 0) Then

                                Dim N1 As Integer = (i - 1) * (nps + 1) + j
                                Dim N2 As Integer = i * (nps + 1) + j
                                Dim N3 As Integer = N2 + 1
                                Dim N4 As Integer = N1 + 1

                                Dim q4 As New Panel(N1, N2, N3, N4)

                                q4.IsPrimitive = False
                                q4.IsSlender = False

                                Mesh.Panels.Add(q4)

                            End If

                        Next

                    Next

                Else

                    ' If there are feature lines:

                    Dim dz As Double = l / npz
                    Dim p As Integer = 0
                    Dim g As Integer = Grids.Count - 1

                    For i = 0 To g

                        ' Add panels between first the section and the first grid

                        Dim zo As Double
                        Dim lz As Double

                        If i = 0 Then

                            zo = CrossSections(0).Z
                            lz = Grids(0).zo - zo

                            Dim nz As Integer = Math.Ceiling(lz / dz)

                            For k = 0 To nz - 1

                                Dim z As Double = zo + lz * k / nz

                                For j = 0 To nps

                                    Dim s As Double = j / nps + (Grids(0).Nodes(j).Y - j / nps) * k / nz

                                    Mesh.Nodes.Add(New NodalPoint(GetPoint(z, s)))

                                    If (p > 0) And (j > 0) Then

                                        Dim N1 As Integer = (p - 1) * (nps + 1) + j
                                        Dim N2 As Integer = p * (nps + 1) + j
                                        Dim N3 As Integer = N2 + 1
                                        Dim N4 As Integer = N1 + 1

                                        Dim q4 As New Panel(N1, N2, N3, N4)

                                        q4.IsPrimitive = False
                                        q4.IsSlender = False

                                        Mesh.Panels.Add(q4)

                                    End If

                                Next

                                p += 1

                            Next

                        End If

                        ' Add this grid:

                        Dim q As Integer = 0

                        For k = 0 To Grids(i).nnz - 1

                            For j = 0 To nps

                                Mesh.Nodes.Add(New NodalPoint(GetPoint(Grids(i).Nodes(q).X, Grids(i).Nodes(q).Y)))

                                ' Save the position p when j = e here!

                                If j = Grids(i).e Then

                                    Grids(i).AnchorIndices(k) = Mesh.Nodes.Count

                                End If

                                q += 1

                                If (p > 0) And (j > 0) Then

                                    Dim N1 As Integer = (p - 1) * (nps + 1) + j
                                    Dim N2 As Integer = p * (nps + 1) + j
                                    Dim N3 As Integer = N2 + 1
                                    Dim N4 As Integer = N1 + 1

                                    Dim q4 As New Panel(N1, N2, N3, N4)

                                    q4.IsPrimitive = False
                                    q4.IsSlender = False

                                    Mesh.Panels.Add(q4)

                                End If

                            Next

                            p += 1

                        Next

                        ' Add region between this grid and the next one:

                        If i < g Then

                            zo = Grids(i).zf
                            lz = Grids(i + 1).zo - zo

                            Dim nz As Integer = Math.Ceiling(lz / dz)

                            For k = 1 To nz - 1

                                ' Make a linear interpolation between the last grid line of 
                                ' the previous grid, and the first grid line of the next grid

                                Dim z As Double = k / nz * lz + zo
                                Dim o As Integer = Grids(i).Nodes.Count - nps - 1

                                For j = 0 To nps

                                    Dim s As Double = Grids(i).Nodes(o + j).Y + (Grids(i + 1).Nodes(j).Y - Grids(i).Nodes(o + j).Y) * k / nz

                                    Mesh.Nodes.Add(New NodalPoint(GetPoint(z, s)))

                                    If (p > 0) And (j > 0) Then

                                        Dim N1 As Integer = (p - 1) * (nps + 1) + j
                                        Dim N2 As Integer = p * (nps + 1) + j
                                        Dim N3 As Integer = N2 + 1
                                        Dim N4 As Integer = N1 + 1

                                        Dim q4 As New Panel(N1, N2, N3, N4)

                                        q4.IsPrimitive = False
                                        q4.IsSlender = False

                                        Mesh.Panels.Add(q4)

                                    End If

                                Next

                                p += 1

                            Next

                        End If

                        ' Add region at the end:

                        If i = g Then

                            zo = Grids(i).zf
                            lz = CrossSections(CrossSections.Count - 1).Z - zo

                            Dim nz As Integer = Math.Ceiling(lz / dz)
                            Dim o As Integer = Grids(i).Nodes.Count - nps - 1

                            For k = 1 To nz

                                For j = 0 To nps

                                    Dim z As Double = k / nz * lz + zo
                                    Dim s As Double = Grids(i).Nodes(o + j).Y + (j / nps - Grids(i).Nodes(o + j).Y) * k / nz

                                    Mesh.Nodes.Add(New NodalPoint(GetPoint(z, s)))

                                    If (p > 0) And (j > 0) Then

                                        Dim N1 As Integer = (p - 1) * (nps + 1) + j
                                        Dim N2 As Integer = p * (nps + 1) + j
                                        Dim N3 As Integer = N2 + 1
                                        Dim N4 As Integer = N1 + 1

                                        Dim q4 As New Panel(N1, N2, N3, N4)

                                        q4.IsPrimitive = False
                                        q4.IsSlender = False

                                        Mesh.Panels.Add(q4)

                                    End If

                                Next

                                p += 1

                            Next

                        End If

                    Next

                End If

                ' If the last section has one node, replace rear quad panels by triangles:

                If CrossSections(CrossSections.Count - 1).Vertices.Count = 1 Then

                    For i = 0 To nps - 1

                        Mesh.Nodes.RemoveAt(Mesh.Nodes.Count - 1)
                        Mesh.Panels.RemoveAt(Mesh.Panels.Count - 1)

                    Next

                    Dim m As Integer = Mesh.Nodes.Count

                    For i = 1 To nps

                        Dim panel3 As New Panel(m, m - i, m - i - 1)

                        panel3.IsSlender = False
                        panel3.IsPrimitive = False

                        Mesh.Panels.Add(panel3)

                    Next

                End If

                ' Add the anchors:

                For Each Grid In Grids

                    For i = 0 To Grid.nnz - 1

                        Mesh.Nodes.Add(New NodalPoint(Grid.ParentAnchor.Lines(i).Point.Clone))

                        If i > 0 Then

                            Dim N1 As Integer = Grid.AnchorIndices(i - 1)
                            Dim N2 As Integer = Grid.AnchorIndices(i)
                            Dim N3 As Integer = Mesh.Nodes.Count
                            Dim N4 As Integer = Mesh.Nodes.Count - 1

                            Dim q4 As New Panel(N1, N2, N3, N4)

                            q4.IsPrimitive = i = Grid.nnz - 1
                            q4.IsSlender = True

                            Mesh.Panels.Add(q4)

                        End If

                    Next

                Next

                ' If the first section only has one node, replace frontal quad panels by triangular panels:

                If CrossSections(0).Vertices.Count = 1 Then

                    For i = 0 To nps - 1

                        Mesh.Nodes.RemoveAt(0)
                        Mesh.Panels.RemoveAt(0)

                    Next

                    For i = 0 To Mesh.Panels.Count - 1

                        Mesh.Panels(i).N1 -= nps
                        Mesh.Panels(i).N2 -= nps
                        Mesh.Panels(i).N3 -= nps
                        Mesh.Panels(i).N4 -= nps

                    Next

                    For i = 0 To nps - 1

                        Dim panel3 As New Panel(1, i + 2, i + 3)

                        panel3.IsSlender = False
                        panel3.IsPrimitive = False

                        Mesh.Panels.Insert(0, panel3)

                    Next

                End If

                ' Add symmetric part: new panels are inserted so that symmetric panels are consecutive.

                Dim n As Integer = Mesh.Nodes.Count - 1

                For i = 0 To n

                    Dim point As New NodalPoint(-Mesh.Nodes(i).Position.X, Mesh.Nodes(i).Position.Y, Mesh.Nodes(i).Position.Z) '  vertex.X, vertex.Y, 0

                    Mesh.Nodes.Add(point)

                Next

                Dim r As Integer = Mesh.Panels.Count - 1

                For i = 0 To r

                    Dim NewPanel As Panel

                    Dim index As Integer = 2 * i

                    If Mesh.Panels(index).IsTriangular Then

                        Dim N1 As Integer = Mesh.Panels(index).N1
                        Dim N2 As Integer = Mesh.Panels(index).N2
                        Dim N3 As Integer = Mesh.Panels(index).N3

                        Mesh.Panels(index).N1 = N1
                        Mesh.Panels(index).N2 = N2
                        Mesh.Panels(index).N3 = N3

                        NewPanel = New Panel(N1 + n + 1, N2 + n + 1, N3 + n + 1)

                    Else

                        Dim N1 As Integer = Mesh.Panels(index).N1
                        Dim N2 As Integer = Mesh.Panels(index).N2
                        Dim N3 As Integer = Mesh.Panels(index).N3
                        Dim N4 As Integer = Mesh.Panels(index).N4

                        Mesh.Panels(index).N1 = N1
                        Mesh.Panels(index).N2 = N2
                        Mesh.Panels(index).N3 = N3
                        Mesh.Panels(index).N4 = N4

                        NewPanel = New Panel(N1 + n + 1, N2 + n + 1, N3 + n + 1, N4 + n + 1)

                    End If

                    NewPanel.IsSlender = Mesh.Panels(index).IsSlender
                    NewPanel.IsPrimitive = Mesh.Panels(index).IsPrimitive

                    ' We don't reverse slender panels (because the same is done in wings, and this simplifies the canvection):

                    NewPanel.IsReversed = Not NewPanel.IsSlender

                    Mesh.Panels.Insert(2 * i + 1, NewPanel)

                Next

                ' Change indices to 0-based

                For i = 0 To Mesh.Panels.Count - 1

                    Mesh.Panels(i).GlobalIndex = i

                Next

                For i = 0 To NumberOfPanels - 1

                    Mesh.Panels(i).N1 -= 1
                    Mesh.Panels(i).N2 -= 1
                    Mesh.Panels(i).N3 -= 1
                    Mesh.Panels(i).N4 -= 1

                Next

                ' Generate lattice

                Mesh.GenerateLattice()

                ' Rotate to align with global XYZ

                For Each p In Mesh.Nodes

                    Dim x As Double = p.Position.X
                    Dim y As Double = p.Position.Y
                    Dim z As Double = p.Position.Z

                    p.Position.X = z
                    p.Position.Y = x
                    p.Position.Z = y

                Next

                Mesh.Rotate(CenterOfRotation, Orientation.InRadians)
                Mesh.Translate(Position)

                ' Local base:

                Dim LocalRotationMatrix As New RotationMatrix

                LocalRotationMatrix.Generate(Orientation.InRadians)

                LocalDirections.U.X = 1.0
                LocalDirections.U.Y = 0.0
                LocalDirections.U.Z = 0.0
                LocalDirections.U.Rotate(LocalRotationMatrix)

                LocalDirections.V.X = 0.0
                LocalDirections.V.Y = 1.0
                LocalDirections.V.Z = 0.0
                LocalDirections.V.Rotate(LocalRotationMatrix)

                LocalDirections.W.X = 0.0
                LocalDirections.W.Y = 0.0
                LocalDirections.W.Z = 1.0
                LocalDirections.W.Rotate(LocalRotationMatrix)

                ' Generate cross sections to display:

                ReDim _CrossSectionsToDisplay(CrossSections.Count - 1)

                For i = 0 To CrossSections.Count - 1
                    ReDim _CrossSectionsToDisplay(i)(2 * CrossSections(i).Vertices.Count - 1)
                Next

                Dim csIndex As Integer = 0

                For Each CrossSection In CrossSections

                    Dim vIndex As Integer = 0

                    For Each Vertex In CrossSection.Vertices

                        _CrossSectionsToDisplay(csIndex)(vIndex) = New Vector3(CrossSection.Z, Vertex.X, Vertex.Y)

                        vIndex += 1

                    Next

                    vIndex = _CrossSectionsToDisplay(csIndex).Length - 1

                    For Each Vertex In CrossSection.Vertices

                        _CrossSectionsToDisplay(csIndex)(vIndex) = New Vector3(CrossSection.Z, -Vertex.X, Vertex.Y)

                        vIndex -= 1

                    Next

                    csIndex += 1

                Next

            Catch

                Mesh.Panels.Clear()
                Mesh.Nodes.Clear()

            End Try

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Returns a clone of the fuselage (not implemented).
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overrides Function Clone() As Surface

            Return Nothing

        End Function

#Region " Anchors "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' For each anchor line, this procedure updates the projection of the associated
        ' wing root nodes on the fuselage surface.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private Sub UpdateAnchors()

            For Each Anchor In AnchorLines

                Anchor.Projections.Clear()

                Dim OutOfBody As Boolean = False

                For Each Line In Anchor.Lines

                    Dim Point As Vector3 = Line.Point

                    Dim currentZ As Double = Point.Z

                    Dim currentP As Vector2 = Nothing

                    Dim lastP As New Vector2(Point.X, Point.Y)

                    ' Find intersection iteratively:

                    Dim iterations = 10

                    For k = 1 To iterations

                        If Not OutOfBody Then

                            If k > 1 Then

                                ' Adjust Zn so that it is closer to the real intersection:

                                Dim dx As Double = currentP.X - lastP.X
                                Dim dy As Double = currentP.Y - lastP.Y

                                currentZ += Line.Direction.Z * Math.Sqrt(dx * dx + dy * dy)

                                lastP.X = currentP.X
                                lastP.Y = currentP.Y

                            End If

                            ' generate a local section:

                            Dim bIndex As Integer = 0 ' begin section index
                            Dim eIndex As Integer = 0 ' end section index

                            OutOfBody = True

                            For i = 0 To CrossSections.Count - 2

                                If currentZ >= CrossSections(i).Z And currentZ < CrossSections(i + 1).Z Then

                                    OutOfBody = False
                                    bIndex = i
                                    eIndex = i + 1
                                    Exit For

                                End If

                            Next

                            ' generate an intermediate section:

                            Dim InterSection As New CrossSection

                            InterSection.Z = currentZ

                            Dim s As Double = 0
                            Dim z As Double = (currentZ - CrossSections(bIndex).Z) / (CrossSections(eIndex).Z - CrossSections(bIndex).Z)
                            Dim Resolution As Integer = 250

                            For i = 0 To Resolution

                                s = i / Resolution

                                Dim bPoint As Vector2 = CrossSections(bIndex).GetPoint(s)
                                Dim ePoint As Vector2 = CrossSections(eIndex).GetPoint(s)

                                Dim iPoint As New Vector2((1 - z) * bPoint.X + z * ePoint.X, (1 - z) * bPoint.Y + z * ePoint.Y)

                                InterSection.Vertices.Add(iPoint)

                            Next

                            InterSection.CalculatePerimeter()

                            ' search the crossing point on the intermediate section:

                            Dim inPlanePoint As New Vector2(Point.X, Point.Y)
                            Dim inPlaneDirection As New Vector2(Line.Direction.X, Line.Direction.Y)
                            Dim intersectionCoordinates As New List(Of Double)
                            Dim intersectionPoints As New List(Of Vector2)
                            Dim indices As New List(Of Integer)
                            s = 0

                            For i = 0 To InterSection.Vertices.Count - 2

                                Dim coordinate As Double = Vector2.IntersectionCoordinate(inPlanePoint, inPlaneDirection, InterSection.Vertices(i), InterSection.Vertices(i + 1))
                                Dim increment As Double = InterSection.Vertices(i).DistanceTo(InterSection.Vertices(i + 1))

                                If Not Double.IsNaN(coordinate) Then

                                    intersectionCoordinates.Add(s + coordinate * increment)
                                    intersectionPoints.Add(New Vector2(InterSection.Vertices(i).X + coordinate * (InterSection.Vertices(i + 1).X - InterSection.Vertices(i).X),
                                                                        InterSection.Vertices(i).Y + coordinate * (InterSection.Vertices(i + 1).Y - InterSection.Vertices(i).Y)))
                                    indices.Add(i)

                                End If

                                s += increment

                            Next

                            ' Pick the intersection closest to the point to be projected:

                            Dim currentS As Double = Double.NaN
                            Dim distance As Double = 0
                            Dim shortestDistance As Double = Double.MaxValue

                            For i = 0 To indices.Count - 1

                                distance = InterSection.Vertices(indices(i)).DistanceTo(inPlanePoint)
                                If distance < shortestDistance Then
                                    shortestDistance = distance
                                    currentS = intersectionCoordinates(i)
                                    currentP = intersectionPoints(i)
                                End If

                            Next

                            If Double.IsNaN(currentS) Then

                                OutOfBody = True
                                Exit For

                            End If

                            If Not OutOfBody And k = iterations Then

                                Dim anchorPoint As New AnchorPoint
                                anchorPoint.s = currentS
                                anchorPoint.Z = currentZ
                                anchorPoint.Perimeter = InterSection.Perimeter
                                Anchor.Projections.Add(anchorPoint)

                            End If

                        Else

                            Exit For

                        End If

                    Next

                Next

                If OutOfBody Then

                    Anchor.Projections.Clear()

                End If

            Next

            ' Updates the anchors in increasing order:

            AnchorLines.Sort(Function(x As AnchorLine, y As AnchorLine)
                                 If x.Projections.Count = 0 Then
                                     Return 0
                                 ElseIf y.Projections.Count = 0 Then
                                     Return 0
                                 ElseIf x.Projections(0).Z > y.Projections(0).Z Then
                                     Return 1
                                 Else
                                     Return -1
                                 End If
                             End Function)

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

#Region " IO "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads the fuselage from an XML file.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overrides Sub ReadFromXML(ByRef Stream As XmlReader)

            While Stream.Read

                Select Case Stream.Name

                    Case "Identity"

                        Name = Stream.GetAttribute("Name")
                        Id = New Guid(IOXML.ReadString(Stream, "ID", Guid.NewGuid.ToString))
                        IncludeInCalculation = IOXML.ReadBoolean(Stream, "Include", True)

                    Case "SurfaceProperties"

                        MeshType = IOXML.ReadInteger(Stream, "MeshType", MeshTypes.StructuredQuadrilaterals)
                        CrossRefinement = IOXML.ReadInteger(Stream, "NPS", 10)
                        LongitudinalRefinement = IOXML.ReadInteger(Stream, "NPZ", 10)

                        Position.X = IOXML.ReadDouble(Stream, "X", 0.0#)
                        Position.Y = IOXML.ReadDouble(Stream, "Y", 0.0#)
                        Position.Z = IOXML.ReadDouble(Stream, "Z", 0.0#)

                        Orientation.Angle1 = IOXML.ReadDouble(Stream, "Psi", 0.0)
                        Orientation.Angle2 = IOXML.ReadDouble(Stream, "Tita", 0.0)
                        Orientation.Angle3 = IOXML.ReadDouble(Stream, "Fi", 0.0)
                        Orientation.Sequence = IOXML.ReadInteger(Stream, "Sequence", CInt(RotationSequence.ZYX))

                        CenterOfRotation.X = IOXML.ReadDouble(Stream, "Xcr", 0.0)
                        CenterOfRotation.Y = IOXML.ReadDouble(Stream, "Ycr", 0.0)
                        CenterOfRotation.Z = IOXML.ReadDouble(Stream, "Zcr", 0.0)

                    Case "CrossSection"

                        CrossSections.Add(New CrossSection(Stream))

                    Case "VisualProperties"

                        VisualProperties.ReadFromXML(Stream.ReadSubtree)

                    Case "Anchor"

                        AnchorLines.Add(New AnchorLine(Stream.ReadSubtree))

                    Case "Inertia"

                        Dim I As InertialProperties

                        I.Mass = IOXML.ReadDouble(Stream, "Mass", 0.0)

                        I.Xcg = IOXML.ReadDouble(Stream, "Xcg", 0.0)
                        I.Ycg = IOXML.ReadDouble(Stream, "Ycg", 0.0)
                        I.Zcg = IOXML.ReadDouble(Stream, "Zcg", 0.0)

                        I.Ixx = IOXML.ReadDouble(Stream, "Ixx", 0.0)
                        I.Iyy = IOXML.ReadDouble(Stream, "Iyy", 0.0)
                        I.Izz = IOXML.ReadDouble(Stream, "Izz", 0.0)

                        I.Ixy = IOXML.ReadDouble(Stream, "Ixy", 0.0)
                        I.Ixz = IOXML.ReadDouble(Stream, "Ixz", 0.0)
                        I.Iyz = IOXML.ReadDouble(Stream, "Iyz", 0.0)

                        Inertia = I

                End Select

            End While

            GenerateMesh()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes the fuselage to an XML file.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Overrides Sub WriteToXML(ByRef Stream As XmlWriter)

            ' Identity
            '-----------------------------------------------------

            Stream.WriteStartElement("Identity")

            Stream.WriteAttributeString("Name", Name)
            Stream.WriteAttributeString("ID", Id.ToString)
            Stream.WriteAttributeString("Include", String.Format("{0}", IncludeInCalculation))

            Stream.WriteEndElement()

            ' Surface properties
            '-----------------------------------------------------

            Stream.WriteStartElement("SurfaceProperties")

            Stream.WriteAttributeString("MeshType", CInt(MeshType))

            Stream.WriteAttributeString("NPS", CInt(CrossRefinement))
            Stream.WriteAttributeString("NPZ", CInt(LongitudinalRefinement))

            Stream.WriteAttributeString("X", String.Format("{0}", Position.X))
            Stream.WriteAttributeString("Y", String.Format("{0}", Position.Y))
            Stream.WriteAttributeString("Z", String.Format("{0}", Position.Z))

            Stream.WriteAttributeString("Psi", String.Format("{0}", Orientation.Angle1))
            Stream.WriteAttributeString("Tita", String.Format("{0}", Orientation.Angle2))
            Stream.WriteAttributeString("Fi", String.Format("{0}", Orientation.Angle3))
            Stream.WriteAttributeString("Sequence", String.Format("{0}", CInt(Orientation.Sequence)))

            Stream.WriteAttributeString("Xcr", String.Format("{0}", CenterOfRotation.X))
            Stream.WriteAttributeString("Ycr", String.Format("{0}", CenterOfRotation.Y))
            Stream.WriteAttributeString("Zcr", String.Format("{0}", CenterOfRotation.Z))

            Stream.WriteEndElement()

            ' Sections
            '-----------------------------------------------------

            For Each Section In CrossSections

                Stream.WriteStartElement("CrossSection")
                Section.WriteToXML(Stream)
                Stream.WriteEndElement()

            Next

            ' Anchors
            '-----------------------------------------------------

            For Each Anchor In AnchorLines

                Stream.WriteStartElement("Anchor")
                Anchor.WriteToXML(Stream)
                Stream.WriteEndElement()

            Next

            ' Visual properties
            '-----------------------------------------------------

            Stream.WriteStartElement("VisualProperties")
            VisualProperties.WriteToXML(Stream)
            Stream.WriteEndElement()

            ' Inertia
            '-----------------------------------------------------

            Stream.WriteStartElement("Inertia")

            Stream.WriteAttributeString("Mass", String.Format("{0,14:E6}", Inertia.Mass))

            Stream.WriteAttributeString("Xcg", String.Format("{0,14:E6}", Inertia.Xcg))
            Stream.WriteAttributeString("Ycg", String.Format("{0,14:E6}", Inertia.Ycg))
            Stream.WriteAttributeString("Zcg", String.Format("{0,14:E6}", Inertia.Zcg))

            Stream.WriteAttributeString("Ixx", String.Format("{0,14:E6}", Inertia.Ixx))
            Stream.WriteAttributeString("Iyy", String.Format("{0,14:E6}", Inertia.Iyy))
            Stream.WriteAttributeString("Izz", String.Format("{0,14:E6}", Inertia.Izz))

            Stream.WriteAttributeString("Ixy", String.Format("{0,14:E6}", Inertia.Ixy))
            Stream.WriteAttributeString("Ixz", String.Format("{0,14:E6}", Inertia.Ixz))
            Stream.WriteAttributeString("Iyz", String.Format("{0,14:E6}", Inertia.Iyz))

            Stream.WriteEndElement()

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

#Region " Examples "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a simple example.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub GenerateExample()

            CrossSections.Clear()

            ' section 0:

            Dim Section0 As New CrossSection
            Section0.Z = 0
            Dim nSegments As Integer = 15

            For i = 0 To nSegments

                Dim angle As Double = i / nSegments * Math.PI
                Section0.Vertices.Add(New Vector2(0.1 * Math.Sin(angle), 0.1 * Math.Cos(angle)))

            Next

            Section0.CalculatePerimeter()

            CrossSections.Add(Section0)

            ' section 1:

            Dim Section1 As New CrossSection
            Section1.Z = 1.5
            nSegments = 15

            For i = 0 To nSegments

                Dim angle As Double = i / nSegments * Math.PI
                Section1.Vertices.Add(New Vector2(1.0 * Math.Sin(angle), 1.0 * Math.Cos(angle)))

            Next

            Section1.CalculatePerimeter()

            CrossSections.Add(Section1)

            ' section 2:

            Dim Section2 As New CrossSection
            Section2.Z = 3
            nSegments = 15

            For i = 0 To nSegments

                Dim angle As Double = i / nSegments * Math.PI
                Section2.Vertices.Add(New Vector2(1.0 * Math.Sin(angle), 1.0 * Math.Cos(angle)))

            Next

            Section2.CalculatePerimeter()

            CrossSections.Add(Section2)

            ' section 3:

            Dim Section3 As New CrossSection
            Section3.Z = 4.5
            nSegments = 15

            For i = 0 To nSegments

                Dim angle As Double = i / nSegments * Math.PI
                Section3.Vertices.Add(New Vector2(0.1 * Math.Sin(angle), 0.1 * Math.Cos(angle)))

            Next

            Section3.CalculatePerimeter()

            CrossSections.Add(Section3)

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a simple model for testing and quality check.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub GenerateModelForTesting_1()

            Mesh.Nodes.Clear()
            Mesh.Nodes.Add(New NodalPoint(2, 0, 0))
            Mesh.Nodes.Add(New NodalPoint(0, 1, 1))
            Mesh.Nodes.Add(New NodalPoint(0, -1, 1))
            Mesh.Nodes.Add(New NodalPoint(0, -1, -1))
            Mesh.Nodes.Add(New NodalPoint(0, 1, -1))
            Mesh.Nodes.Add(New NodalPoint(-2, 0, 0))

            Mesh.Panels.Clear()
            Mesh.Panels.Add(New Panel(1, 2, 3))
            Mesh.Panels.Add(New Panel(1, 3, 4))
            Mesh.Panels.Add(New Panel(1, 4, 5))
            Mesh.Panels.Add(New Panel(1, 5, 2))
            Mesh.Panels.Add(New Panel(6, 3, 2))
            Mesh.Panels.Add(New Panel(6, 4, 3))
            Mesh.Panels.Add(New Panel(6, 5, 4))
            Mesh.Panels.Add(New Panel(6, 2, 5))

            For Each p In Mesh.Panels
                p.IsPrimitive = False
                p.IsSlender = False
            Next

            Mesh.GenerateLattice()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a simple model for testing and quality check.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub GenerateModelForTesting_2()

            Mesh.Nodes.Clear()

            Dim n As Integer = 26

            Mesh.Nodes.Add(New NodalPoint(2, 0, 0))

            For i = 0 To n - 1
                Dim angle = 2 * i / (n - 1) * Math.PI
                Mesh.Nodes.Add(New NodalPoint(0, Math.Cos(angle), Math.Sin(angle)))
            Next

            Mesh.Nodes.Add(New NodalPoint(-2, 0, 0))

            Mesh.Panels.Clear()

            For i = 2 To n - 1

                Mesh.Panels.Add(New Panel(1, i, i + 1))

            Next

            Mesh.Panels.Add(New Panel(1, n, 2))

            For i = 2 To n - 1

                Mesh.Panels.Add(New Panel(n + 2, i + 1, i))

            Next

            Mesh.Panels.Add(New Panel(n + 2, 2, n))

            For Each p In Mesh.Panels
                p.IsPrimitive = False
                p.IsSlender = False
            Next

            Mesh.GenerateLattice()

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Generates a simple model for testing and quality check.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub GenerateModelForTesting_3()

            Mesh.Nodes.Clear()
            Mesh.Nodes.Add(New NodalPoint(0, 0, 0))

            Dim n As Integer = 4
            Dim r As Double = 1

            For i = 0 To n - 1
                Dim angle As Double = 2 * Math.PI * i / n
                Mesh.Nodes.Add(New NodalPoint(2, r * Math.Cos(angle), r * Math.Sin(angle)))
            Next

            For i = 0 To n - 1
                Dim angle As Double = 2 * Math.PI * i / n
                Mesh.Nodes.Add(New NodalPoint(4, r * Math.Cos(angle), r * Math.Sin(angle)))
            Next

            Mesh.Nodes.Add(New NodalPoint(6, 0, 0))

            Mesh.Panels.Clear()

            For i = 1 To n

                If i < n Then
                    Mesh.Panels.Add(New Panel(1, i + 1, i + 2))
                Else
                    Mesh.Panels.Add(New Panel(1, n + 1, 2))
                End If

            Next

            For i = 1 To n

                If i < n Then
                    Mesh.Panels.Add(New Panel(i + 1, i + n + 1, i + n + 2, i + 2))
                Else
                    Mesh.Panels.Add(New Panel(n + 1, 2 * n + 1, n + 2, 2))
                End If

            Next

            For i = 1 To n

                If i < n Then
                    Mesh.Panels.Add(New Panel(2 * n + 2, i + n + 2, i + n + 1))
                Else
                    Mesh.Panels.Add(New Panel(2 * n + 2, n + 2, 2 * n + 1))
                End If

            Next

            'Mesh.Panels.Add(New Panel(2, 6, 7, 3))
            'Mesh.Panels.Add(New Panel(3, 7, 8, 4))
            'Mesh.Panels.Add(New Panel(4, 8, 9, 5))
            'Mesh.Panels.Add(New Panel(5, 9, 6, 2))

            'Mesh.Panels.Add(New Panel(10, 7, 6))
            'Mesh.Panels.Add(New Panel(10, 8, 7))
            'Mesh.Panels.Add(New Panel(10, 9, 8))
            'Mesh.Panels.Add(New Panel(10, 6, 9))

            For Each p In Mesh.Panels
                p.IsPrimitive = False
                p.IsSlender = False
            Next

            Mesh.GenerateLattice()

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class

End Namespace

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

'' Standard .NET frameworks dependencies
'-----------------------------------------------------------------------------
Imports System.Xml

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.AeroTools.Models.Aero.Components
Imports VGL.AeroTools.Models.Structural.Library.Elements
Imports VGL.AeroTools.IoHelper
Imports VGL.AeroTools.Settings
Imports VGL.MathTools.Algebra.EuclideanSpace
Imports VGL.MathTools.Algebra.CustomMatrices
Imports VGL.DesignTools.Interface
Imports VGL.DesignTools.Models.Components.Basics
Imports VGL.DesignTools.DataStore

'#############################################################################
' Unit: LiftingSurface
'
' This unit provides a lifting surface model based on the definition of 
' spanwise regions. Inside each region the profile and polar family is
' considered constant.
'#############################################################################
Namespace DesignTools.Models.Components

    ''' <summary>
    ''' Represents a spanwise region on a wing.
    ''' </summary>
    Public Class WingRegion

#Region " Geometry "

        ''' <summary>
        ''' All posible kind of panel spacement along the wingspan.
        ''' </summary>
        Public Enum Spacements As Integer

            Constant = 1
            Linear = 2

        End Enum

        ''' <summary>
        ''' Number of panels along the wingspan.
        ''' </summary>
        Private _SpanPanels As Integer

        ''' <summary>
        ''' Number of nodes along the wingspan.
        ''' </summary>
        Private _SpanNodes As Integer

        ''' <summary>
        ''' Generates a new wing region object with minimal default values.
        ''' </summary>
        Public Sub New()

            FlapChord = 0.3
            PolarId = Guid.Empty
            SpacementType = Spacements.Constant

        End Sub

        ''' <summary>
        ''' Gets all the constent from an existing wing region.
        ''' </summary>
        ''' <param name="PanelExt"></param>
        ''' <remarks></remarks>
        Public Sub Assign(ByVal PanelExt As WingRegion)

            _SpanPanels = PanelExt.SpanPanelsCount
            _TipChord = PanelExt.TipChord
            _Sweepback = PanelExt.Sweepback
            _Length = PanelExt.Length
            _Dihedral = PanelExt.Dihedral
            _Twist = PanelExt.Twist
            _TwistAxis = PanelExt.TwistAxis
            _FlapChord = PanelExt._FlapChord
            _CenterOfShear = PanelExt.CenterOfShear

        End Sub

        ''' <summary>
        ''' Number of panels in spanwise direction.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property SpanPanelsCount As Integer
            Get
                Return _SpanPanels
            End Get
            Set(ByVal value As Integer)
                _SpanPanels = value
            End Set
        End Property

        ''' <summary>
        ''' Number of nodes in chordwise direction.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property ChordNodesCount As Integer
            Get
                Return _SpanPanels + 1
            End Get
            Set(ByVal value As Integer)
                _SpanPanels = value - 1
            End Set
        End Property

        ''' <summary>
        ''' Tip chord length.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property TipChord As Double

        ''' <summary>
        ''' Spanwise length.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property Length As Double

        ''' <summary>
        ''' Sweep angle.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property Sweepback As Double

        ''' <summary>
        ''' Dihedral angle.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property Dihedral As Double

        ''' <summary>
        ''' Twist angle along the span.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property Twist As Double

        ''' <summary>
        ''' Chordwise position of the twisting axis as fraction of the local chord.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property TwistAxis As Double

        ''' <summary>
        ''' Describes the form of the cord.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property CamberLineId As Guid

        ''' <summary>
        ''' Indicates if this region is flapped.
        ''' </summary>
        ''' <returns></returns>
        Public Property Flapped As Boolean

        ''' <summary>
        ''' Lenght of the flap in chordwise direction.
        ''' </summary>
        ''' <returns></returns>
        Public Property FlapChord As Double

        ''' <summary>
        ''' Deflection of the flap (in rad).
        ''' </summary>
        ''' <returns></returns>
        Public Property FlapDeflection As Double

        ''' <summary>
        ''' Type of spanwise spacing between the stations.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property SpacementType As Spacements

        ''' <summary>
        ''' Index of polar curve to be loaded.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property PolarId As Guid

#End Region

#Region " Structure "

        ''' <summary>
        ''' Represents the section at the tip of the panel
        ''' </summary>
        ''' <remarks></remarks>
        Public Property TipSection As Section = New Section

        ''' <summary>
        ''' The relative position of the center of shear for this region.
        ''' The center of shear is materialized by the position of the structural beam.
        ''' </summary>
        Private _CenterOfShear As Double = 0.25

        ''' <summary>
        ''' The length of the leading edge as computed during the meshing
        ''' </summary>
        ''' <returns></returns>
        Public Property LeadingEdgeLength As Double

        ''' <summary>
        ''' Stablishes the position of the center of shear in relation to the local chord (flap not included)
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property CenterOfShear
            Set(ByVal value)
                _CenterOfShear = value
            End Set
            Get
                Return _CenterOfShear
            End Get
        End Property

#End Region

#Region " Polars "

        ''' <summary>
        ''' Local polar curve.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property PolarFamiliy As PolarFamily

#End Region

#Region " IO "

        ''' <summary>
        ''' Writes the content to an XML file.
        ''' </summary>
        ''' <param name="writer"></param>
        ''' <remarks></remarks>
        Public Sub WriteToXML(ByRef writer As XmlWriter)

            writer.WriteAttributeString("SpanVortexRings", String.Format("{0}", SpanPanelsCount))
            writer.WriteAttributeString("ExternalChord", String.Format("{0}", TipChord))
            writer.WriteAttributeString("Length", String.Format("{0}", Length))
            writer.WriteAttributeString("SweepBack", String.Format("{0}", Sweepback))
            writer.WriteAttributeString("Diedral", String.Format("{0}", Dihedral))
            writer.WriteAttributeString("Torsion", String.Format("{0}", Twist))
            writer.WriteAttributeString("TorsionAxis", String.Format("{0}", TwistAxis))
            writer.WriteAttributeString("SpacingType", String.Format("{0}", SpacementType))

            writer.WriteAttributeString("Flapped", Flapped)
            writer.WriteAttributeString("FlapChord", FlapChord)
            writer.WriteAttributeString("FlapDeflection", FlapDeflection)

            writer.WriteAttributeString("Area", String.Format("{0}", TipSection.AE))
            writer.WriteAttributeString("Iu", String.Format("{0}", TipSection.GJ))
            writer.WriteAttributeString("Iv", String.Format("{0}", TipSection.EIy))
            writer.WriteAttributeString("Iw", String.Format("{0}", TipSection.EIz))
            writer.WriteAttributeString("Sv", String.Format("{0}", TipSection.Cmy))
            writer.WriteAttributeString("Sw", String.Format("{0}", TipSection.Cmz))
            writer.WriteAttributeString("J", String.Format("{0}", TipSection.Ip))
            writer.WriteAttributeString("m", String.Format("{0}", TipSection.M))

            writer.WriteAttributeString("CenterOfShear", String.Format("{0}", CenterOfShear))
            writer.WriteAttributeString("PolarID", String.Format("{0}", PolarId))
            writer.WriteAttributeString("CamberLineID", String.Format("{0}", CamberLineId))

        End Sub

        ''' <summary>
        ''' Reads the content from an XML file.
        ''' </summary>
        ''' <param name="reader"></param>
        ''' <remarks></remarks>
        Public Sub ReadFromXML(ByRef reader As XmlReader)

            SpanPanelsCount = IOXML.ReadInteger(reader, "SpanVortexRings", 4)
            TipChord = IOXML.ReadDouble(reader, "ExternalChord", 1.0)
            Length = IOXML.ReadDouble(reader, "Length", 1.0)
            Sweepback = IOXML.ReadDouble(reader, "SweepBack", 0.0)
            Dihedral = IOXML.ReadDouble(reader, "Diedral", 0.0)
            Twist = IOXML.ReadDouble(reader, "Torsion", 0.0)
            TwistAxis = IOXML.ReadDouble(reader, "TorsionAxis", 0.0)
            SpacementType = [Enum].Parse(GetType(Spacements), IOXML.ReadString(reader, "SpacingType", String.Format("{0}", Spacements.Constant)))

            Flapped = IOXML.ReadBoolean(reader, "Flapped", False)
            FlapChord = IOXML.ReadDouble(reader, "FlapChord", 0.2)
            FlapDeflection = IOXML.ReadDouble(reader, "FlapDeflection", 0.0#)

            TipSection.AE = IOXML.ReadDouble(reader, "Area", 1000)
            TipSection.GJ = IOXML.ReadDouble(reader, "Iu", 1000)
            TipSection.EIy = IOXML.ReadDouble(reader, "Iv", 1000)
            TipSection.EIz = IOXML.ReadDouble(reader, "Iw", 1000)
            TipSection.Ip = IOXML.ReadDouble(reader, "J", 1000)
            TipSection.Cmy = IOXML.ReadDouble(reader, "Sv", 1000)
            TipSection.Cmz = IOXML.ReadDouble(reader, "Sw", 1000)
            TipSection.M = IOXML.ReadDouble(reader, "m", 10)

            CenterOfShear = IOXML.ReadDouble(reader, "CenterOfShear", 0.0)
            PolarId = New Guid(IOXML.ReadString(reader, "PolarID", Guid.Empty.ToString))
            CamberLineId = New Guid(IOXML.ReadString(reader, "CamberLineID", Guid.Empty.ToString))

        End Sub

#End Region

    End Class

    ''' <summary>
    ''' Represents wings
    ''' </summary>
    Public Class LiftingSurface

        Inherits Surface

        ''' <summary>
        ''' Permite bloquear la edicion del contenido
        ''' </summary>
        ''' <remarks></remarks>
        Public Property Lock As Boolean = False

        ''' <summary>
        ''' Position of the local origin in global coordinates.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property LocalOrigin As New Vector3

        ''' <summary>
        ''' Cached points used to represent the directions of the local axes.
        ''' </summary>
        Public Property MainDirections As New Base3

        ''' <summary>
        ''' Indicates if the mesh has to be symmetric about the plane y = 0.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property Symmetric As Boolean = False

        ''' <summary>
        ''' Indicates if the wake has to be convected.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property ConvectWake As Boolean = True

        ''' <summary>
        ''' Indicates if the wake should be convected by the trailing edge only.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property TrailingEdgeConvection As Boolean
            Set(ByVal value As Boolean)
                _TrailingEdgeConvection = value
                RefreshPrimitives()
            End Set
            Get
                Return _TrailingEdgeConvection
            End Get
        End Property

        ''' <summary>
        ''' The index of the first panel in the current region
        ''' </summary>
        ''' <returns></returns>
        Public ReadOnly Property CurrentRegionStartPanel As Integer

        ''' <summary>
        ''' The index of the first panel in the current region
        ''' </summary>
        ''' <returns></returns>
        Public ReadOnly Property CurrentRegionEndPanel As Integer

        ''' <summary>
        ''' Indicates at which time-step the wake has to be trimmed.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property CuttingStep As Integer = 50

        ''' <summary>
        ''' The number of panels in the chorwise direction
        ''' </summary>
        Private _ChordPanelsCount As Integer = 5

        ''' <summary>
        ''' The total number of panels in the spanwise direction
        ''' This is obtained while generating the mesh by summing the span panels in all of the regions.
        ''' </summary>
        Private _SpanPanelsCount As Integer

        ''' <summary>
        ''' The number of panels along the chord.
        ''' </summary>
        Private _ChordNodesCount As Integer

        ''' <summary>
        ''' The total number of nodes in the spanwise direction.
        ''' This is obtained while generating the mesh by summing the span panels in all of the regions.
        ''' </summary>
        Private _SpanNodesCount As Integer

        ''' <summary>
        ''' The total number of nodes along the boundary.
        ''' </summary>
        Private _BoundaryNodesCount As Integer

        ''' <summary>
        ''' The total number of segments along the boundary.
        ''' </summary>
        Private _BoundarySegmentsCount As Integer

        ''' <summary>
        ''' The total number of primitive segments along the convection edge.
        ''' </summary>
        Private _PrimitivePanelsCount As Integer

        ''' <summary>
        ''' Te total number of primitive nodes along the convection edge.
        ''' </summary>
        Private _PrimitiveNodesCount As Integer

        ''' <summary>
        ''' When set, it will setup the convection along the primitive edge.
        ''' </summary>
        Private _TrailingEdgeConvection As Boolean = True

        ''' <summary>
        ''' Contains the wing regions.
        ''' </summary>
        ''' <returns></returns>
        Public Property WingRegions As New List(Of WingRegion)

        ''' <summary>
        ''' The index of the active region
        ''' </summary>
        Private _CurrentWingRegion As Integer = 0

        ''' <summary>
        ''' The wing root chord [m]
        ''' </summary>
        Private _RootChord As Double

        ''' <summary>
        ''' The relative length of the flap relative to the wing chord
        ''' </summary>
        Private _RootFlapChord As Double

        ''' <summary>
        ''' The number of flapped panels in chordwise direction
        ''' </summary>
        Private _FlapPanels As Integer

        ''' <summary>
        ''' This dynamic array contains the boundary nodes, which for a lifting surface are
        ''' all adjacent along the edge.
        ''' </summary>
        Private _BoundaryNodes(1)

        ''' <summary>
        ''' This dynamic array contains the boundary panels, which for a lifting surface are
        ''' all adjacent along the edge.
        ''' </summary>
        Private _BoundaryPanels(1)

        ''' <summary>
        ''' This static array stores the indices of the panels and nodes being the limits of the convection edge.
        ''' </summary>
        Private _PrimitiveData(2, 2) As Integer ' Comienzo y fin del borde de conveccion (paneles y nodos)

        ''' <summary>
        ''' The index of the first primirive panel
        ''' </summary>
        Private _PrimitivePanel1 As Integer

        ''' <summary>
        ''' The index of the last primitive panel
        ''' </summary>
        Private _PrimitivePanel2 As Integer

        ''' <summary>
        ''' Geerates a new lfting surface object with default values.
        ''' </summary>
        Public Sub New()

            Mesh = New Mesh()

            Name = "Lifting surface"
            Id = Guid.NewGuid

            NumberOfChordPanels = 5
            RootChord = 1.0#
            RootFlap = 0.2
            FlapPanels = 3
            Symmetric = True
            IncludeInCalculation = True
            InitializeRegions()

            FirstPrimitiveSegment = 1
            LastPrimitiveSegment = 1

            FirstPrimitiveSegment = NumberOfChordPanels + 1
            LastPrimitiveSegment = FirstPrimitiveSegment + WingRegions(0).SpanPanelsCount - 1

            _CurrentRegionStartPanel = 0
            _CurrentRegionEndPanel = 0

            GenerateMesh()

            VisualProperties = New VisualProperties(ComponentTypes.etLiftingSurface)

        End Sub

#Region " Geometry and mesh properties "

        ''' <summary>
        ''' Number of chordwise panels
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property NumberOfChordPanels As Integer
            Get
                Return _ChordPanelsCount
            End Get
            Set(ByVal value As Integer)
                _ChordPanelsCount = value
            End Set
        End Property

        ''' <summary>
        ''' Number of spanwise panels.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property NumberOfSpanPanels As Integer
            Get
                Return _SpanPanelsCount
            End Get
        End Property

        ''' <summary>
        ''' Length of the root chord.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property RootChord As Double
            Get
                Return _RootChord
            End Get
            Set(ByVal value As Double)
                _RootChord = value
            End Set
        End Property

        ''' <summary>
        ''' Length of the root flap (as fraction of the root chord).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property RootFlap As Double
            Set(ByVal value As Double)
                _RootFlapChord = value
            End Set
            Get
                Return _RootFlapChord
            End Get
        End Property

        ''' <summary>
        ''' Number of chordwise flap panels.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property FlapPanels As Integer
            Set(ByVal value As Integer)
                If value < NumberOfChordPanels Then
                    _FlapPanels = value
                Else
                    _FlapPanels = NumberOfChordPanels - 1
                End If
            End Set
            Get
                Return _FlapPanels
            End Get
        End Property

        ''' <summary>
        ''' Indicates which regions is currently selected or selects an existing region.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property CurrentRegionId As Integer
            Get
                Return _CurrentWingRegion + 1
            End Get
            Set(ByVal value As Integer)
                If value >= 1 And value <= WingRegions.Count Then
                    _CurrentWingRegion = value - 1
                End If
                UpdateLimitPanels()
            End Set
        End Property

        ''' <summary>
        ''' Indicates if the panel index is contained in the current region
        ''' </summary>
        ''' <param name="PanelIndex"></param>
        ''' <returns></returns>
        Public ReadOnly Property OnCurrentRegion(PanelIndex) As Boolean
            Get
                Return PanelIndex >= CurrentRegionStartPanel And PanelIndex <= CurrentRegionEndPanel
            End Get
        End Property


#End Region

#Region " Convective border and primitive segements "

        ''' <summary>
        ''' Number of segments around the perimeter.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property NumBoundarySegments
            Get
                Return _BoundarySegmentsCount
            End Get
        End Property

        ''' <summary>
        ''' Number of boundary nodes.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property NumBoundaryNodes
            Get
                Return _BoundaryNodesCount
            End Get
        End Property

        ''' <summary>
        ''' Number of primitive segments.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property NumPrimitiveSegments
            Get
                RefreshPrimitives()
                Return _PrimitivePanelsCount
            End Get
        End Property

        ''' <summary>
        ''' Number of primitive nodes.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property NumPrimitiveNodes
            Get
                RefreshPrimitives()
                Return _PrimitiveNodesCount
            End Get
        End Property

        ''' <summary>
        ''' Position of the segment defined as firt primitive.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property FirstPrimitiveSegment As Integer
            Get
                Return _PrimitiveData(1, 1)
            End Get
            Set(ByVal value As Integer)
                _PrimitiveData(1, 1) = value
                RefreshPrimitives()
            End Set
        End Property

        ''' <summary>
        ''' Position of the segment defined as last primitive.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property LastPrimitiveSegment As Integer
            Get
                Return _PrimitiveData(1, 2)
            End Get
            Set(ByVal value As Integer)
                Me._PrimitiveData(1, 2) = value
                Me.RefreshPrimitives()
            End Set
        End Property

        ''' <summary>
        ''' Position of the node defined as first primitive (depends on the first primitive segment).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property FirstPrimitiveNode As Integer
            Get
                Return Me._PrimitiveData(2, 1)
            End Get
        End Property

        ''' <summary>
        ''' Position of the node defined as last primitive (depends on the last primitive segment).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property LastPrimitiveNode As Integer
            Get
                Return Me._PrimitiveData(2, 2)
            End Get
        End Property

        ''' <summary>
        ''' Returns the index of a given primitive node.
        ''' </summary>
        ''' <param name="Node"></param>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property GetPrimitiveNodeIndex(ByVal Node As Integer) As Integer
            Get
                If Node >= 1 And Node <= Me._BoundaryNodesCount Then
                    Return Me._BoundaryNodes(Node)
                Else
                    Return 1
                End If
            End Get
        End Property

        ''' <summary>
        ''' Returns the location of a given primitive node.
        ''' </summary>
        ''' <param name="Nodo"></param>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property GetPrimitiveNodePosition(ByVal Nodo As Integer) As Vector3
            Get
                If Nodo >= 1 And Nodo <= Me._BoundaryNodesCount Then
                    Return Mesh.Nodes(_BoundaryNodes(Nodo)).Position
                Else
                    Return New Vector3
                End If
            End Get
        End Property

        ''' <summary>
        ''' Returns the index of a given primitive segment.
        ''' </summary>
        ''' <param name="Segment"></param>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property GetPrimitivePanelIndex(ByVal Segment As Integer) As Integer
            Get
                If Segment >= 1 And Segment <= Me._BoundarySegmentsCount Then
                    Return _BoundaryPanels(Segment)
                Else
                    Return 1
                End If
            End Get
        End Property

        ''' <summary>
        ''' Returns the panel associated to a given primitive segment.
        ''' </summary>
        ''' <param name="Segmento"></param>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property GetPrimitivePanel(ByVal Segmento As Integer) As Panel
            Get
                If Segmento >= 1 And Segmento <= Me._BoundarySegmentsCount Then
                    Return Mesh.Panels(Me._BoundaryPanels(Segmento) - 1)
                Else
                    Return Mesh.Panels(0)
                End If
            End Get
        End Property

#End Region

#Region " Wing regions management "

        ''' <summary>
        ''' Recalculates the limits of the current region
        ''' </summary>
        Private Sub UpdateLimitPanels()

            _CurrentRegionStartPanel = 0
            _CurrentRegionEndPanel = 0

            For Each Region In WingRegions

                If Region IsNot CurrentRegion Then

                    _CurrentRegionStartPanel += NumberOfChordPanels * Region.SpanPanelsCount

                Else

                    _CurrentRegionEndPanel = _CurrentRegionStartPanel + NumberOfChordPanels * Region.SpanPanelsCount

                    _CurrentRegionStartPanel += 1

                    Return

                End If

            Next

        End Sub

        ''' <summary>
        ''' Loads a simple wing region.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub InitializeRegions()

            Dim NewRegion As New WingRegion
            NewRegion.SpanPanelsCount = 5
            NewRegion.TipChord = 1.0#
            NewRegion.Length = 1.0#
            NewRegion.TwistAxis = 0.5#
            WingRegions.Add(NewRegion)

        End Sub

        ''' <summary>
        ''' Adds a new region at the end.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub AddRegion()

            Dim NewRegion As New WingRegion
            NewRegion.SpanPanelsCount = 5
            NewRegion.TipChord = 1.0#
            NewRegion.Length = 1.0#
            NewRegion.TwistAxis = 0.5#
            WingRegions.Add(NewRegion)

        End Sub

        ''' <summary>
        ''' Insets a new wing region at a given position.
        ''' </summary>
        ''' <param name="Position"></param>
        ''' <remarks></remarks>
        Public Overloads Sub InsertRegion(ByVal Position As Integer)

            If Position >= 1 And Position <= WingRegions.Count Then

                Dim NewRegion As New WingRegion
                NewRegion.SpanPanelsCount = 5
                NewRegion.TipChord = 1.0#
                NewRegion.Length = 1.0#
                NewRegion.TwistAxis = 0.5#
                WingRegions.Insert(Position - 1, NewRegion)

            End If

        End Sub

        ''' <summary>
        ''' Inserts a new wing region after the current one.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overloads Sub InsertRegion()

            Dim NewRegion As New WingRegion
            NewRegion.SpanPanelsCount = 5
            NewRegion.TipChord = 1.0#
            NewRegion.Length = 1.0#
            NewRegion.TwistAxis = 0.5#
            WingRegions.Insert(_CurrentWingRegion, NewRegion)

        End Sub

        ''' <summary>
        ''' Removes the current wing region.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub RemoveCurrentRegion()

            If WingRegions.Count > 1 Then

                WingRegions.RemoveAt(_CurrentWingRegion)
                _CurrentWingRegion = Math.Max(0, _CurrentWingRegion - 1)

            End If

        End Sub

        ''' <summary>
        ''' Currently active region
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public ReadOnly Property CurrentRegion As WingRegion
            Get
                Return WingRegions.Item(_CurrentWingRegion)
            End Get
        End Property

#End Region

#Region " 3D model and vortices generation "

        ''' <summary>
        ''' Generates the lifting surface mesh based on the geometrical parameters.
        ''' </summary>
        Public Overrides Sub GenerateMesh()

            Dim X As Double
            Dim Chord As Double
            Dim ChordStart As Double
            Dim ChordEnd As Double
            Dim RegionSpan As Double
            Dim StripSpan As Double
            Dim StageSpan As Double
            Dim StageChord As Double
            Dim StripSpanFraction As Double
            Dim StageTipFraction As Double
            Dim StageRootFraction As Double
            Dim B1 As Double
            Dim C1 As Double
            Dim B2 As Double
            Dim C2 As Double
            Dim D2 As Double
            Dim E As Double
            Dim Ttwist As Double
            Dim MrotA(3, 3) As Double
            Dim Pij As New Vector3
            Dim Phi As Double
            Dim BasePoint As New Vector3

            ' Determine the number of elements
            '---------------------------------------------------------------------

            _ChordNodesCount = _ChordPanelsCount + 1

            _SpanNodesCount = 1

            _SpanPanelsCount = 0

            For i = 0 To WingRegions.Count - 1

                _SpanNodesCount = WingRegions.Item(i).ChordNodesCount + _SpanNodesCount

                _SpanPanelsCount = WingRegions.Item(i).SpanPanelsCount + _SpanPanelsCount

            Next

            _BoundaryNodesCount = 2 * _ChordNodesCount + 2 * _SpanNodesCount

            _BoundarySegmentsCount = 2 * _ChordPanelsCount + 2 * _SpanPanelsCount

            RefreshPrimitives()

            ' Clear the mesh
            '---------------------------------------------------------------------

            Mesh.Nodes.Clear()

            Mesh.Panels.Clear()

            ' Load quad panels (base on indices only)
            '---------------------------------------------------------------------

            For p = 1 To _SpanPanelsCount

                For q = 0 To _ChordPanelsCount - 1

                    Dim Panel As New Panel

                    Panel.N1 = (p - 1) * _ChordNodesCount + q
                    Panel.N2 = (p - 1) * _ChordNodesCount + q + 1
                    Panel.N3 = p * _ChordNodesCount + q + 1
                    Panel.N4 = p * _ChordNodesCount + q

                    Mesh.Panels.Add(Panel)

                Next

            Next

            ' Find the indices of the boundary nodes and panels
            '---------------------------------------------------------------------

            ReDim _BoundaryNodes(_BoundaryNodesCount)

            ReDim _BoundaryPanels(_BoundarySegmentsCount)

            Dim s As Integer

            s = 1

            For q = 1 To _ChordPanelsCount + 1
                _BoundaryNodes(s) = q
                s = s + 1
            Next

            For p = 1 To _SpanPanelsCount - 1
                _BoundaryNodes(s) = (p + 1) * (_ChordPanelsCount + 1)
                s = s + 1
            Next

            For q = 1 To _ChordPanelsCount + 1
                _BoundaryNodes(s) = (_ChordPanelsCount + 1) * (_SpanPanelsCount + 1) - (q - 1)
                s = s + 1
            Next

            For p = 1 To _SpanPanelsCount - 1
                _BoundaryNodes(s) = (_SpanPanelsCount + 1 - p) * (_ChordPanelsCount + 1) - _ChordPanelsCount
                s = s + 1
            Next

            _BoundaryNodes(_BoundaryNodesCount) = 1

            s = 1

            For p = 1 To _ChordPanelsCount
                _BoundaryPanels(s) = p
                s = s + 1
            Next

            For p = 1 To _SpanPanelsCount
                _BoundaryPanels(s) = _ChordPanelsCount * p
                s = s + 1
            Next

            For p = 1 To _ChordPanelsCount
                _BoundaryPanels(s) = _ChordPanelsCount * _SpanPanelsCount - (p - 1)
                s = s + 1
            Next

            For p = 1 To _SpanPanelsCount
                _BoundaryPanels(s) = _ChordPanelsCount * _SpanPanelsCount - p * _ChordPanelsCount + 1
                s = s + 1
            Next

            ' Build rotation matrix
            '---------------------------------------------------------------------

            Dim LocalRotationMatrix As New RotationMatrix

            LocalRotationMatrix.Generate(Orientation.InRadians)

            ' Locate root chord nodes
            '---------------------------------------------------------------------

            Dim NodeCounter As Integer = 1

            Dim FlapChord As Double = RootFlap

            Dim RootCamber As CamberLine = GetCamberLineFromId(WingRegions(0).CamberLineId)

            For i = 1 To _ChordNodesCount

                If WingRegions(0).Flapped Then

                    If i <= _ChordNodesCount - FlapPanels Then

                        X = (i - 1) / (_ChordNodesCount - FlapPanels - 1) * (1 - FlapChord)

                    Else

                        X = (1 - FlapChord) + (i - _ChordNodesCount + FlapPanels) / FlapPanels * FlapChord

                    End If

                Else

                    X = (i - 1) / _ChordPanelsCount

                End If

                Dim Point As New Vector2

                Dim deflection As Single = WingRegions(0).FlapDeflection

                WingRegions(0).FlapDeflection = 0

                RootCamber.EvaluatePoint(Point, X)

                Point.X *= _RootChord

                Point.Y *= _RootChord

                WingRegions(0).FlapDeflection = deflection

                Dim Node As New NodalPoint(Point.X, 0, Point.Y)

                If Symmetric Then Node.Position.Y = -Node.Position.Y

                Node.Position.Substract(CenterOfRotation)

                Node.Position.Rotate(LocalRotationMatrix)

                Node.Position.Add(CenterOfRotation)

                Node.Position.Add(Position)

                Mesh.Nodes.Add(Node)

                NodeCounter += 1

            Next

            ' Start building the geometry of each wing region
            '---------------------------------------------------------------------

            Dim RootLeadingEdgePoint As New Vector3
            Dim PreviousLeadingEdgePoint As Vector3 = Nothing

            Dim Twist As Double = 0.0#

            For RegionIndex = 0 To WingRegions.Count - 1

                ' Initialize the common variables for this region

                WingRegions(RegionIndex).LeadingEdgeLength = 0.0#

                Dim Delta As Double = WingRegions(RegionIndex).Sweepback / 180.0 * Math.PI

                Dim Gamma As Double = WingRegions(RegionIndex).Dihedral / 180.0 * Math.PI

                ' Initialize the origin of this region

                If RegionIndex = 0 Then

                    RootLeadingEdgePoint.X = 0.0#

                    RootLeadingEdgePoint.Y = 0.0#

                    RootLeadingEdgePoint.Z = 0.0#

                    Twist = 0.0#

                Else

                    RootLeadingEdgePoint.X = BasePoint.X

                    RootLeadingEdgePoint.Y = BasePoint.Y

                    RootLeadingEdgePoint.Z = BasePoint.Z

                    Twist = (Twist + WingRegions(RegionIndex - 1).Twist / 180.0 * Math.PI) * Math.Cos(Gamma)

                End If

                ' Compute chord lenghts at the base and tip of this region

                If RegionIndex = 0 Then

                    ChordStart = _RootChord

                    ChordEnd = WingRegions(RegionIndex).TipChord

                Else

                    ChordStart = WingRegions(RegionIndex - 1).TipChord

                    ChordEnd = WingRegions(RegionIndex).TipChord

                End If

                ' Compute other helping variables

                RegionSpan = WingRegions(RegionIndex).Length

                StageTipFraction = 1 - 1 / WingRegions.Item(RegionIndex).SpanPanelsCount

                StageSpan = RegionSpan - ChordEnd / _ChordPanelsCount

                StageRootFraction = 1 / WingRegions.Item(RegionIndex).SpanPanelsCount

                StageChord = ChordStart / _ChordPanelsCount

                C1 = (StageSpan - RegionSpan * StageTipFraction) / (StageTipFraction ^ 2 - StageTipFraction)

                B1 = RegionSpan - C1

                E = ((StageSpan - RegionSpan * StageTipFraction) - (StageTipFraction ^ 3 - StageTipFraction)) / (StageTipFraction ^ 2 - StageTipFraction)

                D2 = (StageChord - RegionSpan * StageRootFraction) / (StageRootFraction ^ 3 + E * StageRootFraction ^ 2 - StageRootFraction * (E + 1))

                C2 = E * D2

                B2 = RegionSpan - C2 - D2

                ' Generate the geometry of each chordwise strip
                '------------------------------------------------------------------

                For k = 1 To WingRegions(RegionIndex).SpanPanelsCount

                    ' a) Calculate the distance to the column in spanwise direction

                    StripSpanFraction = k / WingRegions.Item(RegionIndex).SpanPanelsCount

                    Select Case WingRegions.Item(RegionIndex).SpacementType

                        Case WingRegion.Spacements.Constant

                            StripSpan = RegionSpan * StripSpanFraction

                        Case WingRegion.Spacements.Linear

                            StripSpan = B1 * StripSpanFraction + C1 * StripSpanFraction ^ 2

                    End Select

                    ' b) Calculate the local chord

                    Chord = ChordStart + (ChordEnd - ChordStart) * StripSpan / RegionSpan

                    ' c) Calculate the position of the twisting axis

                    If RegionIndex = 0 Then

                        Ttwist = WingRegions(RegionIndex).TwistAxis

                    Else

                        Ttwist = WingRegions(RegionIndex - 1).TwistAxis + (WingRegions.Item(RegionIndex - 1).TwistAxis - WingRegions.Item(RegionIndex - 1).TwistAxis) * StripSpan / RegionSpan

                    End If

                    ' d) Calculate the twisting angle

                    Phi = WingRegions(RegionIndex).Twist * StripSpanFraction / 180.0 * Math.PI

                    ' d) Calculate the properties of the flap

                    Dim FlapChordStart As Single

                    Dim FlapChordEnd As Single = WingRegions(RegionIndex).FlapChord

                    If RegionIndex = 0 Then
                        FlapChordStart = RootFlap
                    Else
                        FlapChordStart = WingRegions(RegionIndex - 1).FlapChord
                    End If

                    Dim LocalCamber As CamberLine = CamberLinesDatabase.GetCamberLineFromId(WingRegions(RegionIndex).CamberLineId)

                    ' Locate each chordwise node in the strip

                    For l = 1 To _ChordNodesCount

                        ' Calculate the distance from the nodal point to the leading edge

                        FlapChord = FlapChordStart + (FlapChordEnd - FlapChordStart) * StripSpanFraction

                        If WingRegions(RegionIndex).Flapped Then

                            WingRegions(RegionIndex).FlapChord = FlapChord

                            If l <= _ChordNodesCount - FlapPanels Then

                                X = (l - 1) / (_ChordNodesCount - FlapPanels - 1) * (1 - FlapChord)

                            Else

                                X = (1 - FlapChord) + (l - _ChordNodesCount + FlapPanels) / FlapPanels * FlapChord

                            End If

                        Else

                            X = (l - 1) / _ChordPanelsCount

                        End If

                        ' Evaluate the point in the camber line

                        Dim Point As New Vector2

                        LocalCamber.EvaluatePoint(Point, X, WingRegions(RegionIndex).Flapped, WingRegions(RegionIndex).FlapChord, WingRegions(RegionIndex).FlapDeflection)

                        Point.X *= Chord

                        Point.Y *= Chord

                        ' Apply local twist

                        Pij.X = Math.Cos(Phi) * (Point.X - Ttwist * Chord) + Math.Sin(Phi) * Point.Y + StripSpan * Math.Tan(Delta) + Ttwist * Chord

                        Pij.Y = StripSpan

                        Pij.Z = -Math.Sin(Phi) * (Point.X - Ttwist * Chord) + Math.Cos(Phi) * Point.Y

                        ' Apply overal twist

                        Dim Node As New NodalPoint

                        Node.Position.X = Pij.X * Math.Cos(Twist) + Pij.Z * Math.Sin(Twist) + RootLeadingEdgePoint.X

                        Node.Position.Y = Pij.X * Math.Sin(Gamma) * Math.Sin(Twist) + Pij.Y * Math.Cos(Gamma) - Pij.Z * Math.Sin(Gamma) * Math.Cos(Twist) + RootLeadingEdgePoint.Y

                        Node.Position.Z = -Pij.X * Math.Cos(Gamma) * Math.Sin(Twist) + Pij.Y * Math.Sin(Gamma) + Pij.Z * Math.Cos(Gamma) * Math.Cos(Twist) + RootLeadingEdgePoint.Z

                        If k = WingRegions(RegionIndex).SpanPanelsCount And l = 1 Then

                            BasePoint.X = Node.Position.X

                            BasePoint.Y = Node.Position.Y

                            BasePoint.Z = Node.Position.Z

                        End If

                        ' Global positioning:

                        Node.Position.Substract(CenterOfRotation)

                        Node.Position.Rotate(LocalRotationMatrix)

                        Node.Position.Add(CenterOfRotation)

                        Node.Position.Add(Position)

                        Mesh.Nodes.Add(Node)

                        ' Increase leading edge length:

                        If l = 1 Then

                            If PreviousLeadingEdgePoint IsNot Nothing Then

                                WingRegions(RegionIndex).LeadingEdgeLength += PreviousLeadingEdgePoint.DistanceTo(Node.Position)

                            End If

                            PreviousLeadingEdgePoint = Node.Position

                        End If

                        ' Increase node counter

                        NodeCounter += 1

                    Next

                    WingRegions(RegionIndex).FlapChord = FlapChordEnd

                Next

            Next

            ' Assign property to primitive panels

            For i = FirstPrimitiveSegment To LastPrimitiveSegment
                Mesh.Panels(GetPrimitivePanelIndex(i) - 1).IsPrimitive = True
            Next

            Mesh.GenerateLattice()

            ' Local base
            '------------------------

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

            ' Direction points:
            '------------------------

            MainDirections.U.X = 0.5
            MainDirections.U.Y = 0.0
            MainDirections.U.Z = 0.0
            MainDirections.U.Substract(CenterOfRotation)
            MainDirections.U.Rotate(LocalRotationMatrix)
            MainDirections.U.Add(CenterOfRotation)
            MainDirections.U.Add(Position)

            MainDirections.V.X = 0.0
            MainDirections.V.Y = 0.5
            MainDirections.V.Z = 0.0
            MainDirections.V.Substract(CenterOfRotation)
            MainDirections.V.Rotate(LocalRotationMatrix)
            MainDirections.V.Add(CenterOfRotation)
            MainDirections.V.Add(Position)

            MainDirections.W.X = 0.0
            MainDirections.W.Y = 0.0
            MainDirections.W.Z = 0.5
            MainDirections.W.Substract(CenterOfRotation)
            MainDirections.W.Rotate(LocalRotationMatrix)
            MainDirections.W.Add(CenterOfRotation)
            MainDirections.W.Add(Position)

            ' Local origin
            '------------------------

            LocalOrigin.SetToCero()
            LocalOrigin.Substract(CenterOfRotation)
            LocalOrigin.Rotate(LocalRotationMatrix)
            LocalOrigin.Add(CenterOfRotation)
            LocalOrigin.Add(Position)

            GenerateStructure()

            ' Region limits
            '------------------------

            UpdateLimitPanels()

            ' Launch base sub to raise update event

            MyBase.GenerateMesh()

        End Sub

        ''' <summary>
        ''' Refreshes the primitive node and panel indices.
        ''' </summary>
        Private Sub RefreshPrimitives()

            If TrailingEdgeConvection Then
                _PrimitiveData(1, 1) = NumberOfChordPanels + 1
                _PrimitiveData(1, 2) = FirstPrimitiveNode + NumberOfSpanPanels - 1
            End If

            ' First primitive node:
            _PrimitiveData(2, 1) = _PrimitiveData(1, 1)
            ' Last primitive node:
            _PrimitiveData(2, 2) = _PrimitiveData(1, 2) + 1

            ' Number of primitive nodes
            _PrimitiveNodesCount = _PrimitiveData(2, 2) - _PrimitiveData(2, 1) + 1
            ' Number of primitive panels
            _PrimitivePanelsCount = _PrimitiveData(1, 2) - _PrimitiveData(1, 1) + 1

        End Sub

#End Region

#Region " Structure "

        ''' <summary>
        ''' Determines whether or not the structure will be included on the calculation.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property IncludeStructure As Boolean = True

        ''' <summary>
        ''' Wing root section.
        ''' </summary>
        ''' <remarks></remarks>
        Public Property RootSection As Section = New Section

        ''' <summary>
        ''' Contains all nodes and segments representing the structure.
        ''' </summary>
        ''' <remarks></remarks>
        Private _StructuralPartition As New List(Of StructuralPartition)

        ''' <summary>
        ''' Represents the structural nodal points
        ''' </summary>
        ''' <remarks></remarks>
        Public ReadOnly Property StructuralPartition As List(Of StructuralPartition)
            Get
                Return _StructuralPartition
            End Get
        End Property

        ''' <summary>
        ''' Generates the nodal partition and the section partition.
        ''' </summary>
        ''' <remarks></remarks>
        Private Sub GenerateStructure()

            _StructuralPartition.Clear()

            Dim LeadingEdgePoint As Vector3
            Dim TrailingEdgePoint As Vector3
            Dim NodalStripIndex As Integer = 0
            Dim RootSection As Section = Me.RootSection
            Dim RootChord As Double = Me.RootChord
            Dim PreviousLeadingEdgePoint As Vector3 = Nothing

            ' Build partition
            '-------------------------------------

            Dim First As Integer = 0

            For Each Panel As WingRegion In WingRegions

                Dim Length As Double = 0.0

                ' Create the panel partition
                '-------------------------------------

                For P = First To Panel.SpanPanelsCount

                    LeadingEdgePoint = Mesh.Nodes(NodalStripIndex * _ChordNodesCount).Position

                    If Panel.Flapped Then
                        TrailingEdgePoint = Mesh.Nodes((NodalStripIndex + 1) * _ChordNodesCount - 1 - FlapPanels).Position
                    Else
                        TrailingEdgePoint = Mesh.Nodes((NodalStripIndex + 1) * _ChordNodesCount - 1).Position
                    End If

                    Dim StructuralNode As New Vector3

                    StructuralNode.X = LeadingEdgePoint.X + Panel.CenterOfShear * (TrailingEdgePoint.X - LeadingEdgePoint.X)
                    StructuralNode.Y = LeadingEdgePoint.Y + Panel.CenterOfShear * (TrailingEdgePoint.Y - LeadingEdgePoint.Y)
                    StructuralNode.Z = LeadingEdgePoint.Z + Panel.CenterOfShear * (TrailingEdgePoint.Z - LeadingEdgePoint.Z)

                    Dim LocalPartition = New StructuralPartition

                    LocalPartition.P.X = StructuralNode.X
                    LocalPartition.P.Y = StructuralNode.Y
                    LocalPartition.P.Z = StructuralNode.Z

                    If PreviousLeadingEdgePoint IsNot Nothing Then
                        Length += PreviousLeadingEdgePoint.DistanceTo(LeadingEdgePoint)
                    End If

                    Dim S As Double = Length / Panel.LeadingEdgeLength

                    LocalPartition.LocalSection.AE = RootSection.AE + S * (Panel.TipSection.AE - RootSection.AE)
                    LocalPartition.LocalSection.GJ = RootSection.GJ + S * (Panel.TipSection.GJ - RootSection.GJ)
                    LocalPartition.LocalSection.EIy = RootSection.EIy + S * (Panel.TipSection.EIy - RootSection.EIy)
                    LocalPartition.LocalSection.EIz = RootSection.EIz + S * (Panel.TipSection.EIz - RootSection.EIz)
                    LocalPartition.LocalSection.Ip = RootSection.Ip + S * (Panel.TipSection.Ip - RootSection.Ip)
                    LocalPartition.LocalSection.M = RootSection.M + S * (Panel.TipSection.M - RootSection.M)
                    LocalPartition.LocalSection.Cmy = RootSection.Cmy + S * (Panel.TipSection.Cmy - RootSection.Cmy)
                    LocalPartition.LocalSection.Cmz = RootSection.Cmz + S * (Panel.TipSection.Cmz - RootSection.Cmz)
                    LocalPartition.LocalChord = RootChord + S * (Panel.TipChord - RootChord)
                    LocalPartition.LocalSection.Cmy *= LocalPartition.LocalChord
                    LocalPartition.LocalSection.Cmz *= LocalPartition.LocalChord
                    LocalPartition.Basis.V.FromSubstraction(TrailingEdgePoint, LeadingEdgePoint)
                    LocalPartition.Basis.V.Normalize()

                    If (NodalStripIndex > 0) Then

                        Dim PreviousNode As Vector3 = StructuralPartition(StructuralPartition.Count - 1).P

                        LocalPartition.Basis.U.FromSubstraction(StructuralNode, PreviousNode)
                        LocalPartition.Basis.U.Normalize()
                        LocalPartition.Basis.W.FromVectorProduct(LocalPartition.Basis.V, LocalPartition.Basis.U)
                        LocalPartition.Basis.W.Normalize()
                        LocalPartition.Basis.V.FromVectorProduct(LocalPartition.Basis.U, LocalPartition.Basis.W)
                        LocalPartition.Basis.V.Normalize()
                        LocalPartition.M.Assign(LocalPartition.P)
                        LocalPartition.M.Add(LocalPartition.Basis.V, LocalPartition.LocalSection.Cmy)
                        LocalPartition.M.Add(LocalPartition.Basis.W, LocalPartition.LocalSection.Cmz)

                        If (NodalStripIndex = 1) Then

                            ' Go back to the first partition node
                            '------------------------------------
                            Dim FirstPartition As StructuralPartition = StructuralPartition(0)

                            FirstPartition.Basis.U.FromSubstraction(StructuralNode, PreviousNode)
                            FirstPartition.Basis.U.Normalize()
                            FirstPartition.Basis.W.FromVectorProduct(FirstPartition.Basis.V, FirstPartition.Basis.U)
                            FirstPartition.Basis.W.Normalize()
                            FirstPartition.Basis.V.FromVectorProduct(FirstPartition.Basis.U, FirstPartition.Basis.W)
                            FirstPartition.Basis.V.Normalize()
                            FirstPartition.M.Assign(FirstPartition.P)
                            FirstPartition.M.Add(FirstPartition.Basis.V, FirstPartition.LocalSection.Cmy)
                            FirstPartition.M.Add(FirstPartition.Basis.W, FirstPartition.LocalSection.Cmz)

                        End If

                    End If

                    StructuralPartition.Add(LocalPartition)

                    NodalStripIndex += 1

                    PreviousLeadingEdgePoint = LeadingEdgePoint

                Next

                RootSection = Panel.TipSection
                RootChord = Panel.TipChord
                First = 1

            Next

        End Sub

#End Region

#Region " UVLM method "

        ''' <summary>
        ''' Calculates the normal vectors and the control points.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub CalculateControlPointsAndNormals()

            Try

                Dim Nodo1 As New Vector3
                Dim Nodo2 As New Vector3
                Dim Nodo3 As New Vector3
                Dim Nodo4 As New Vector3

                Dim Vector1 As Vector3
                Dim Vector2 As Vector3
                Dim Vector3 As Vector3
                Dim Vector4 As Vector3

                Dim Diagonal1 As New Vector3
                Dim Diagonal2 As New Vector3

                For i = 0 To NumberOfPanels - 1

                    Nodo1 = Mesh.Nodes(Mesh.Panels(i).N1).Position
                    Nodo2 = Mesh.Nodes(Mesh.Panels(i).N2).Position
                    Nodo3 = Mesh.Nodes(Mesh.Panels(i).N3).Position
                    Nodo4 = Mesh.Nodes(Mesh.Panels(i).N4).Position

                    Vector1 = Nodo1.GetVectorToPoint(Nodo2)
                    Vector2 = Nodo2.GetVectorToPoint(Nodo3)
                    Vector3 = Nodo3.GetVectorToPoint(Nodo4)
                    Vector4 = Nodo4.GetVectorToPoint(Nodo1)

                    Mesh.Panels(i).ControlPoint.X = 0.25 * (Nodo1.X + Nodo2.X + Nodo3.X + Nodo4.X)
                    Mesh.Panels(i).ControlPoint.Y = 0.25 * (Nodo1.Y + Nodo2.Y + Nodo3.Y + Nodo4.Y)
                    Mesh.Panels(i).ControlPoint.Z = 0.25 * (Nodo1.Z + Nodo2.Z + Nodo3.Z + Nodo4.Z)

                    Diagonal1.X = Nodo2.X - Nodo4.X
                    Diagonal1.Y = Nodo2.Y - Nodo4.Y
                    Diagonal1.Z = Nodo2.Z - Nodo4.Z

                    Diagonal2.X = Nodo3.X - Nodo1.X
                    Diagonal2.Y = Nodo3.Y - Nodo1.Y
                    Diagonal2.Z = Nodo3.Z - Nodo1.Z

                    Mesh.Panels(i).NormalVector = Algebra.VectorProduct(Diagonal1, Diagonal2).NormalizedDirection
                    Mesh.Panels(i).Area = 0.5 * Algebra.VectorProduct(Vector1, Vector2).Norm2 + 0.5 * Algebra.VectorProduct(Vector3, Vector4).Norm2

                Next

            Catch ex As Exception

                MsgBox("Unable to calculate local data on lifting surface:" & vbCrLf & ex.Message, MsgBoxStyle.OkOnly, "Error while calculating local data")

            End Try

        End Sub

#End Region

#Region " Other methods "

        ''' <summary>
        ''' Generates a copy of the instance.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overrides Function Clone() As Surface

            Dim Surface As New LiftingSurface

            Surface.RootChord = RootChord
            Surface.NumberOfChordPanels = NumberOfChordPanels
            Surface.Symmetric = True
            Surface.FirstPrimitiveSegment = FirstPrimitiveSegment
            Surface.LastPrimitiveSegment = LastPrimitiveSegment
            Surface.ConvectWake = ConvectWake
            Surface.RootSection.Assign(RootSection)

            ' Add panels:

            Dim p As Integer = 0

            For Each Panel In WingRegions

                If p > 0 Then
                    Surface.AddRegion()
                End If

                Surface.WingRegions(p).Assign(Panel)
                Surface.WingRegions(p).TipSection.Assign(WingRegions(p).TipSection)

                p += 1

            Next

            Surface.GenerateMesh()

            Return Surface

        End Function

#End Region

#Region " Geometrical opperations "

        ''' <summary>
        ''' Aligns the surface by means of four reference points (it is not working well).
        ''' </summary>
        ''' <param name="Point1"></param>
        ''' <param name="Point2"></param>
        ''' <param name="Point3"></param>
        ''' <param name="Point4"></param>
        ''' <remarks></remarks>
        Public Overrides Sub Align(ByVal Point1 As Vector3, ByVal Point2 As Vector3, ByVal Point3 As Vector3, ByVal Point4 As Vector3)

            ' Rotate arround P1 in order to align segments:

            Me.CenterOfRotation.X = Point2.X
            Me.CenterOfRotation.Y = Point2.Y
            Me.CenterOfRotation.Z = Point2.Z

            Dim V1 As Vector3 = Point1 - Point3
            Dim V2 As Vector3 = Point2 - Point4

            Dim V1h As New Vector2

            V1h.X = V1.X
            V1h.Y = V1.Y
            V1h.Normalize()

            Dim V2h As New Vector2
            V2h.X = V2.X
            V2h.Y = V2.Y
            V2h.Normalize()

            Dim V2ho As Vector2 = V2h.OrthogonalVector(V1h)
            Dim V1ho As New Vector2
            V1ho.Y = V1h.X
            V1ho.X = -V1h.Y

            Dim Sign As Integer = Math.Sign(V2ho.X * V1ho.X + V2ho.Y * V1ho.Y)

            Orientation.Angle1 += Sign * Math.Acos(V1h.X * V2h.X + V1h.Y * V2h.Y) * 180 / Math.PI

            'Dim Vertical As New EVector3
            'Vertical.Z = V1.Z - V2.Z
            'Me.OrientacionGlobal.Tita = Math.Acos(V2.ProductoInterno(V2 + Vertical)) * 180 / Math.PI

            ' Translate P1 to P2:

            Position.X = Point2.X
            Position.Y = Point2.Y
            Position.Z = Point2.Z

            GenerateMesh()

        End Sub

#End Region

#Region " IO "

        ''' <summary>
        ''' Reads the wing from an XML file.
        ''' </summary>
        ''' <param name="reader"></param>
        ''' <remarks></remarks>
        Public Overrides Sub ReadFromXML(ByRef reader As XmlReader)

            Dim count As Integer = 1

            While reader.Read

                If Not reader.NodeType = XmlNodeType.Element Then Continue While

                Select Case reader.Name

                    Case "Identity"

                        Name = reader.GetAttribute("Name")
                        Id = New Guid(IOXML.ReadString(reader, "ID", Guid.NewGuid.ToString))
                        IncludeInCalculation = IOXML.ReadBoolean(reader, "Include", True)

                    Case "SurfaceProperties"

                        RootChord = IOXML.ReadDouble(reader, "RootChord", 0.0)
                        RootFlap = IOXML.ReadDouble(reader, "RootFlap", 0.2)
                        FlapPanels = IOXML.ReadInteger(reader, "FlapPanels", 3)

                        Position.X = IOXML.ReadDouble(reader, "X", 0.0)
                        Position.Y = IOXML.ReadDouble(reader, "Y", 0.0)
                        Position.Z = IOXML.ReadDouble(reader, "Z", 0.0)

                        Orientation.Angle1 = IOXML.ReadDouble(reader, "Psi", 0.0)
                        Orientation.Angle2 = IOXML.ReadDouble(reader, "Tita", 0.0)
                        Orientation.Angle3 = IOXML.ReadDouble(reader, "Fi", 0.0)
                        Orientation.Sequence = IOXML.ReadInteger(reader, "Sequence", CInt(RotationSequence.ZYX))

                        CenterOfRotation.X = IOXML.ReadDouble(reader, "Xcr", 0.0)
                        CenterOfRotation.Y = IOXML.ReadDouble(reader, "Ycr", 0.0)
                        CenterOfRotation.Z = IOXML.ReadDouble(reader, "Zcr", 0.0)

                        FirstPrimitiveSegment = IOXML.ReadInteger(reader, "Primitive1", 1)
                        LastPrimitiveSegment = IOXML.ReadInteger(reader, "Primitive2", 2)
                        TrailingEdgeConvection = IOXML.ReadBoolean(reader, "TrailingConvection", False)

                        ConvectWake = IOXML.ReadBoolean(reader, "ConvectWake", True)
                        CuttingStep = IOXML.ReadInteger(reader, "CuttingStep", 1)
                        Symmetric = IOXML.ReadBoolean(reader, "Symmetric", True)

                        RootSection.AE = IOXML.ReadDouble(reader, "RootA", 0.0)
                        RootSection.GJ = IOXML.ReadDouble(reader, "RootIu", 0.0)
                        RootSection.EIy = IOXML.ReadDouble(reader, "RootIv", 0.0)
                        RootSection.EIz = IOXML.ReadDouble(reader, "RootIw", 0.0)
                        RootSection.Cmy = IOXML.ReadDouble(reader, "RootSv", 0.0)
                        RootSection.Cmz = IOXML.ReadDouble(reader, "RootSw", 0.0)
                        RootSection.Ip = IOXML.ReadDouble(reader, "RootJ", 0.0)
                        RootSection.M = IOXML.ReadDouble(reader, "Rootm", 0.0)

                        NumberOfChordPanels = IOXML.ReadInteger(reader, "NumberChordRings", 6)

                    Case "MacroPanel", String.Format("MacroPanel{0}", count)

                        If count > 1 Then AddRegion()
                        CurrentRegionId = count
                        CurrentRegion.ReadFromXML(reader)
                        count += 1

                    Case "VisualProperties"

                        VisualProperties.ReadFromXML(reader.ReadSubtree)

                    Case "Inertia"

                        Dim I As InertialProperties

                        I.Mass = IOXML.ReadDouble(reader, "Mass", 0.0)

                        I.Xcg = IOXML.ReadDouble(reader, "Xcg", 0.0)
                        I.Ycg = IOXML.ReadDouble(reader, "Ycg", 0.0)
                        I.Zcg = IOXML.ReadDouble(reader, "Zcg", 0.0)

                        I.Ixx = IOXML.ReadDouble(reader, "Ixx", 0.0)
                        I.Iyy = IOXML.ReadDouble(reader, "Iyy", 0.0)
                        I.Izz = IOXML.ReadDouble(reader, "Izz", 0.0)

                        I.Ixy = IOXML.ReadDouble(reader, "Ixy", 0.0)
                        I.Ixz = IOXML.ReadDouble(reader, "Ixz", 0.0)
                        I.Iyz = IOXML.ReadDouble(reader, "Iyz", 0.0)

                        Inertia = I

                End Select

            End While

            RefreshPrimitives()

            GenerateMesh()

        End Sub

        ''' <summary>
        ''' Writes the wing to an XML file.
        ''' </summary>
        ''' <param name="writer"></param>
        ''' <remarks></remarks>
        Public Overrides Sub WriteToXML(ByRef writer As XmlWriter)

            ' Identity
            '-----------------------------------------------------

            writer.WriteStartElement("Identity")

            writer.WriteAttributeString("Name", Name)
            writer.WriteAttributeString("ID", Id.ToString)
            writer.WriteAttributeString("Include", String.Format("{0}", IncludeInCalculation))

            writer.WriteEndElement()

            ' Surface properties
            '-----------------------------------------------------

            writer.WriteStartElement("SurfaceProperties")

            writer.WriteAttributeString("RootChord", String.Format("{0}", RootChord))
            writer.WriteAttributeString("RootFlap", String.Format("{0}", RootFlap))
            writer.WriteAttributeString("FlapPanels", String.Format("{0}", FlapPanels))

            writer.WriteAttributeString("X", String.Format("{0}", Position.X))
            writer.WriteAttributeString("Y", String.Format("{0}", Position.Y))
            writer.WriteAttributeString("Z", String.Format("{0}", Position.Z))

            writer.WriteAttributeString("Psi", String.Format("{0}", Orientation.Angle1))
            writer.WriteAttributeString("Tita", String.Format("{0}", Orientation.Angle2))
            writer.WriteAttributeString("Fi", String.Format("{0}", Orientation.Angle3))
            writer.WriteAttributeString("Sequence", String.Format("{0}", CInt(Orientation.Sequence)))

            writer.WriteAttributeString("Xcr", String.Format("{0}", CenterOfRotation.X))
            writer.WriteAttributeString("Ycr", String.Format("{0}", CenterOfRotation.Y))
            writer.WriteAttributeString("Zcr", String.Format("{0}", CenterOfRotation.Z))

            writer.WriteAttributeString("Primitive1", String.Format("{0}", FirstPrimitiveSegment))
            writer.WriteAttributeString("Primitive2", String.Format("{0}", LastPrimitiveSegment))
            writer.WriteAttributeString("ConvectWake", String.Format("{0}", CInt(ConvectWake)))
            writer.WriteAttributeString("CuttingStep", String.Format("{0}", CuttingStep))
            writer.WriteAttributeString("Symmetric", String.Format("{0}", CInt(Symmetric)))
            writer.WriteAttributeString("TrailingConvection", String.Format("{0}", CInt(TrailingEdgeConvection)))

            writer.WriteAttributeString("RootA", String.Format("{0}", RootSection.AE))
            writer.WriteAttributeString("RootIu", String.Format("{0}", RootSection.GJ))
            writer.WriteAttributeString("RootIv", String.Format("{0}", RootSection.EIy))
            writer.WriteAttributeString("RootIw", String.Format("{0}", RootSection.EIz))
            writer.WriteAttributeString("RootSv", String.Format("{0}", RootSection.Cmy))
            writer.WriteAttributeString("RootSw", String.Format("{0}", RootSection.Cmz))
            writer.WriteAttributeString("RootJ", String.Format("{0}", RootSection.Ip))
            writer.WriteAttributeString("Rootm", String.Format("{0}", RootSection.M))

            writer.WriteAttributeString("NumberChordRings", String.Format("{0}", NumberOfChordPanels))
            writer.WriteAttributeString("NumberMacroPanels", String.Format("{0}", WingRegions.Count))

            writer.WriteEndElement()

            ' Macro panels
            '-----------------------------------------------------

            For i = 0 To WingRegions.Count - 1

                CurrentRegionId = i

                writer.WriteStartElement("MacroPanel")
                WingRegions(i).WriteToXML(writer)
                writer.WriteEndElement()

            Next

            ' Visual properties
            '-----------------------------------------------------

            writer.WriteStartElement("VisualProperties")
            VisualProperties.WriteToXML(writer)
            writer.WriteEndElement()

            ' Inertia
            '-----------------------------------------------------

            writer.WriteStartElement("Inertia")

            writer.WriteAttributeString("Mass", String.Format("{0,14:E6}", Inertia.Mass))

            writer.WriteAttributeString("Xcg", String.Format("{0,14:E6}", Inertia.Xcg))
            writer.WriteAttributeString("Ycg", String.Format("{0,14:E6}", Inertia.Ycg))
            writer.WriteAttributeString("Zcg", String.Format("{0,14:E6}", Inertia.Zcg))

            writer.WriteAttributeString("Ixx", String.Format("{0,14:E6}", Inertia.Ixx))
            writer.WriteAttributeString("Iyy", String.Format("{0,14:E6}", Inertia.Iyy))
            writer.WriteAttributeString("Izz", String.Format("{0,14:E6}", Inertia.Izz))

            writer.WriteAttributeString("Ixy", String.Format("{0,14:E6}", Inertia.Ixy))
            writer.WriteAttributeString("Ixz", String.Format("{0,14:E6}", Inertia.Ixz))
            writer.WriteAttributeString("Iyz", String.Format("{0,14:E6}", Inertia.Iyz))

            writer.WriteEndElement()

        End Sub

        ''' <summary>
        ''' Writes a ASCII STL file containing the model mesh
        ''' </summary>
        ''' <param name="FilePath"></param>
        Public Overrides Sub ExportStlFile(FilePath As String, Optional Append As Boolean = False, Optional Transformation As Matrix3x3 = Nothing)

            ' Export the normal side

            MyBase.ExportStlFile(FilePath, Append)

            ' Export the symmetric side

            If Symmetric Then

                Dim M As New Matrix3x3
                M.Item(1, 1) = 1.0#
                M.Item(2, 2) = -1.0#
                M.Item(3, 3) = 1.0#

                MyBase.ExportStlFile(FilePath, Append, M)

            End If

        End Sub

        ''' <summary>
        ''' Returns a string with information about this wing.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function RetriveStringData() As String

            Dim Data As String = ""

            Dim Factor As Integer = 1
            If Symmetric Then
                Factor = 2
            End If

            Data += "Mesh data:" & vbCrLf
            Data += String.Format("Total number of nodal points: {0}", Factor * Mesh.Nodes.Count) & vbCrLf
            Data += String.Format("Total number of vortex rings: {0}", Factor * Mesh.Panels.Count) & vbCrLf
            Data += String.Format("Total number of vortex segments: {0}", Factor * Mesh.Lattice.Count) & vbCrLf
            Data += vbCrLf

            Me.CalculateControlPointsAndNormals()

            ' Calculate surface area:

            Dim TotalArea As Double = 0

            For Each Ring In Mesh.Panels

                TotalArea += Ring.Area

            Next

            TotalArea *= Factor

            Data += "Surface data:" & vbCrLf
            Data += String.Format("Total area: {0:F6}m²", TotalArea) & vbCrLf

            ' Calculate surface length:

            Dim TotalWingspan As Double
            Dim ProjectedWingspan As Double
            Dim Index As Integer = 1

            For Each Panel In WingRegions

                TotalWingspan += Panel.Length
                ProjectedWingspan += Panel.Length * Math.Abs(Math.Cos(Math.PI * Panel.Dihedral / 180))
                Data += String.Format(" > region {0:D}: {1:F6}m", Index, Panel.LeadingEdgeLength) & vbCrLf
                Index += 1

            Next

            TotalWingspan *= Factor
            ProjectedWingspan *= Factor

            Data += String.Format("Wingspan: {0:F6}m", TotalWingspan) & vbCrLf
            Data += String.Format("Projected wingspan: {0:F6}m", ProjectedWingspan) & vbCrLf

            ' Aspect ratio:

            Data += String.Format("Aparent aspect ratio: {0:F6}", TotalWingspan ^ 2 / TotalArea) & vbCrLf
            Data += String.Format("Projected aspect ratio: {0:F6}", ProjectedWingspan ^ 2 / TotalArea) & vbCrLf

            Data += "Structural partition:" & vbCrLf

            Index = 1
            For Each Element In StructuralPartition

                Data += String.Format(" > element {0:D}:", Index) & vbCrLf
                Data += String.Format("    AE : {0,7:e4} N/m", Element.LocalSection.AE) & vbCrLf
                Data += String.Format("    EIy: {0,7:e4} N.m/rad", Element.LocalSection.EIy) & vbCrLf
                Data += String.Format("    EIz: {0,7:e4} N.m/rad", Element.LocalSection.EIy) & vbCrLf
                Data += String.Format("    GJ : {0,7:e4} N.m/rad", Element.LocalSection.GJ) & vbCrLf
                Data += String.Format("    m  : {0,7:e4} kg/m", Element.LocalSection.M) & vbCrLf
                Data += String.Format("    Ip : {0,7:e4} kg.m", Element.LocalSection.Ip) & vbCrLf
                Index += 1

            Next

            Return Data

        End Function

#End Region

    End Class

End Namespace
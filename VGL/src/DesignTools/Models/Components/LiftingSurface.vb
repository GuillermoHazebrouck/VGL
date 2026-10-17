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

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a spanwise region on a wing.
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class WingRegion

#Region " Geometry "

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' All posible kind of panel spacement along the wingspan.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Enum Spacements As Integer

            Constant = 1
            Linear = 2

        End Enum

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of panels along the wingspan.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private _SpanPanels As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of nodes along the wingspan.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private _SpanNodes As Integer

        '==========================================================================================
        ' Generates a new wing region object with minimal default values.
        '==========================================================================================
        Public Sub New()

            FlapChord = 0.3
            PolarId = Guid.Empty
            SpacementType = Spacements.Constant

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Gets all the constent from an existing wing region.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of panels in spanwise direction.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SpanPanelsCount As Integer
            Get
                Return _SpanPanels
            End Get
            Set(ByVal value As Integer)
                _SpanPanels = value
            End Set
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of nodes in chordwise direction.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property ChordNodesCount As Integer
            Get
                Return _SpanPanels + 1
            End Get
            Set(ByVal value As Integer)
                _SpanPanels = value - 1
            End Set
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Tip chord length.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property TipChord As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Spanwise length.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Length As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Sweep angle.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Sweepback As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Dihedral angle.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Dihedral As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Twist angle along the span.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Twist As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Chordwise position of the twisting axis as fraction of the local chord.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property TwistAxis As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Describes the form of the cord.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CamberLineId As Guid

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if this region is flapped.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property Flapped As Boolean

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Lenght of the flap in chordwise direction.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FlapChord As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Deflection of the flap (in rad).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FlapDeflection As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Type of spanwise spacing between the stations.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property SpacementType As Spacements

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Index of polar curve to be loaded.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property PolarId As Guid

#End Region

#Region " Structure "

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Represents the section at the tip of the panel
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property TipSection As Section = New Section

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The relative position of the center of shear for this region.
        ' The center of shear is materialized by the position of the structural beam.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Private _CenterOfShear As Double = 0.25

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The length of the leading edge as computed during the meshing
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property LeadingEdgeLength As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stablishes the position of the center of shear in relation to the local chord 
        ' (flap not included)
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property CenterOfShear
            Set(ByVal value)
                _CenterOfShear = value
            End Set
            Get
                Return _CenterOfShear
            End Get
        End Property
        '------------------------------------------------------------------------------------------

#End Region

#Region " Polars "

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local polar curve.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property PolarFamiliy As PolarFamily

#End Region

#Region " IO "

        '==========================================================================================
        ' Writes the content to an XML file.
        '==========================================================================================
        Public Sub WriteToXML(ByRef Stream As XmlWriter)

            Stream.WriteAttributeString("SpanVortexRings", String.Format("{0}", SpanPanelsCount))
            Stream.WriteAttributeString("ExternalChord", String.Format("{0}", TipChord))
            Stream.WriteAttributeString("Length", String.Format("{0}", Length))
            Stream.WriteAttributeString("SweepBack", String.Format("{0}", Sweepback))
            Stream.WriteAttributeString("Diedral", String.Format("{0}", Dihedral))
            Stream.WriteAttributeString("Torsion", String.Format("{0}", Twist))
            Stream.WriteAttributeString("TorsionAxis", String.Format("{0}", TwistAxis))
            Stream.WriteAttributeString("SpacingType", String.Format("{0}", SpacementType))

            Stream.WriteAttributeString("Flapped", Flapped)
            Stream.WriteAttributeString("FlapChord", FlapChord)
            Stream.WriteAttributeString("FlapDeflection", FlapDeflection)

            Stream.WriteAttributeString("Area", String.Format("{0}", TipSection.AE))
            Stream.WriteAttributeString("Iu", String.Format("{0}", TipSection.GJ))
            Stream.WriteAttributeString("Iv", String.Format("{0}", TipSection.EIy))
            Stream.WriteAttributeString("Iw", String.Format("{0}", TipSection.EIz))
            Stream.WriteAttributeString("Sv", String.Format("{0}", TipSection.Cmy))
            Stream.WriteAttributeString("Sw", String.Format("{0}", TipSection.Cmz))
            Stream.WriteAttributeString("J", String.Format("{0}", TipSection.Ip))
            Stream.WriteAttributeString("m", String.Format("{0}", TipSection.M))

            Stream.WriteAttributeString("CenterOfShear", String.Format("{0}", CenterOfShear))
            Stream.WriteAttributeString("PolarID", String.Format("{0}", PolarId))
            Stream.WriteAttributeString("CamberLineID", String.Format("{0}", CamberLineId))

        End Sub
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Reads the content from an XML file.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Sub ReadFromXML(ByRef Stream As XmlReader)

            SpanPanelsCount = IOXML.ReadInteger(Stream, "SpanVortexRings", 4)
            TipChord = IOXML.ReadDouble(Stream, "ExternalChord", 1.0)
            Length = IOXML.ReadDouble(Stream, "Length", 1.0)
            Sweepback = IOXML.ReadDouble(Stream, "SweepBack", 0.0)
            Dihedral = IOXML.ReadDouble(Stream, "Diedral", 0.0)
            Twist = IOXML.ReadDouble(Stream, "Torsion", 0.0)
            TwistAxis = IOXML.ReadDouble(Stream, "TorsionAxis", 0.0)
            SpacementType = [Enum].Parse(GetType(Spacements), IOXML.ReadString(Stream, "SpacingType", String.Format("{0}", Spacements.Constant)))

            Flapped = IOXML.ReadBoolean(Stream, "Flapped", False)
            FlapChord = IOXML.ReadDouble(Stream, "FlapChord", 0.2)
            FlapDeflection = IOXML.ReadDouble(Stream, "FlapDeflection", 0.0#)

            TipSection.AE = IOXML.ReadDouble(Stream, "Area", 1000)
            TipSection.GJ = IOXML.ReadDouble(Stream, "Iu", 1000)
            TipSection.EIy = IOXML.ReadDouble(Stream, "Iv", 1000)
            TipSection.EIz = IOXML.ReadDouble(Stream, "Iw", 1000)
            TipSection.Ip = IOXML.ReadDouble(Stream, "J", 1000)
            TipSection.Cmy = IOXML.ReadDouble(Stream, "Sv", 1000)
            TipSection.Cmz = IOXML.ReadDouble(Stream, "Sw", 1000)
            TipSection.M = IOXML.ReadDouble(Stream, "m", 10)

            CenterOfShear = IOXML.ReadDouble(Stream, "CenterOfShear", 0.0)
            PolarId = New Guid(IOXML.ReadString(Stream, "PolarID", Guid.Empty.ToString))
            CamberLineId = New Guid(IOXML.ReadString(Stream, "CamberLineID", Guid.Empty.ToString))

        End Sub
        '------------------------------------------------------------------------------------------

#End Region

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents wings
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Class LiftingSurface

        Inherits Surface

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Permite bloquear la edicion del contenido
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property Lock As Boolean = False

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Position of the local origin in global coordinates.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property LocalOrigin As New Vector3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Cached points used to represent the directions of the local axes.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property MainDirections As New Base3

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if the mesh has to be symmetric about the plane y = 0.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <value></value>
        ' <returns></returns>
        Public Property Symmetric As Boolean = False

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if the wake has to be convected.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <value></value>
        ' <returns></returns>
        Public Property ConvectWake As Boolean = True

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates if the wake should be convected by the trailing edge only.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <value></value>
        ' <returns></returns>
        Public Property TrailingEdgeConvection As Boolean
            Set(ByVal value As Boolean)
                _TrailingEdgeConvection = value
                RefreshPrimitives()
            End Set
            Get
                Return _TrailingEdgeConvection
            End Get
        End Property

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The index of the first panel in the current region
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <returns></returns>
        Public ReadOnly Property CurrentRegionStartPanel As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The index of the first panel in the current region
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <returns></returns>
        Public ReadOnly Property CurrentRegionEndPanel As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Indicates at which time-step the wake has to be trimmed.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <value></value>
        ' <returns></returns>
        Public Property CuttingStep As Integer = 50

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The number of panels in the chorwise direction
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _ChordPanelsCount As Integer = 5

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The total number of panels in the spanwise direction
        ' This is obtained while generating the mesh by summing the span panels in all of the regions.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _SpanPanelsCount As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The number of panels along the chord.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _ChordNodesCount As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The total number of nodes in the spanwise direction.
        ' This is obtained while generating the mesh by summing the span panels in all of the regions.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _SpanNodesCount As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The total number of nodes along the boundary.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _BoundaryNodesCount As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The total number of segments along the boundary.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _BoundarySegmentsCount As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The total number of primitive segments along the convection edge.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _PrimitivePanelsCount As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Te total number of primitive nodes along the convection edge.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _PrimitiveNodesCount As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' When set, it will setup the convection along the primitive edge.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _TrailingEdgeConvection As Boolean = True

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Contains the wing regions.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <returns></returns>
        Public Property WingRegions As New List(Of WingRegion)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The index of the active region
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _CurrentWingRegion As Integer = 0

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The wing root chord [m]
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _RootChord As Double

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The relative length of the flap relative to the wing chord
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _RootFlapChord As Double

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The number of flapped panels in chordwise direction
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _FlapPanels As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' This dynamic array contains the boundary nodes, which for a lifting surface are
        ' all adjacent along the edge.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _BoundaryNodes(1)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' This dynamic array contains the boundary panels, which for a lifting surface are
        ' all adjacent along the edge.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _BoundaryPanels(1)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' This static array stores the indices of the panels and nodes being the limits of the convection edge.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _PrimitiveData(2, 2) As Integer ' Comienzo y fin del borde de conveccion (paneles y nodos)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The index of the first primirive panel
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _PrimitivePanel1 As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' The index of the last primitive panel
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _PrimitivePanel2 As Integer

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Geerates a new lfting surface object with default values.
        '//////////////////////////////////////////////////////////////////////////////////////////
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
        '------------------------------------------------------------------------------------------

#Region " Geometry and mesh properties "

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of chordwise panels
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property NumberOfChordPanels As Integer
            Get
                Return _ChordPanelsCount
            End Get
            Set(ByVal value As Integer)
                _ChordPanelsCount = value
            End Set
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of spanwise panels.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property NumberOfSpanPanels As Integer
            Get
                Return _SpanPanelsCount
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Length of the root chord.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property RootChord As Double
            Get
                Return _RootChord
            End Get
            Set(ByVal value As Double)
                _RootChord = value
            End Set
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Length of the root flap (as fraction of the root chord).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property RootFlap As Double
            Set(ByVal value As Double)
                _RootFlapChord = value
            End Set
            Get
                Return _RootFlapChord
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of chordwise flap panels.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates which regions is currently selected or selects an existing region.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the panel index is contained in the current region
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property OnCurrentRegion(PanelIndex) As Boolean
            Get
                Return PanelIndex >= CurrentRegionStartPanel And PanelIndex <= CurrentRegionEndPanel
            End Get
        End Property


#End Region

#Region " Convective border and primitive segements "

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of segments around the perimeter.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property NumBoundarySegments
            Get
                Return _BoundarySegmentsCount
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of boundary nodes.
        '------------------------------------------------------------------------------------------
        Public ReadOnly Property NumBoundaryNodes
            Get
                Return _BoundaryNodesCount
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of primitive segments.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property NumPrimitiveSegments
            Get
                RefreshPrimitives()
                Return _PrimitivePanelsCount
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Number of primitive nodes.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property NumPrimitiveNodes
            Get
                RefreshPrimitives()
                Return _PrimitiveNodesCount
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Position of the segment defined as firt primitive.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property FirstPrimitiveSegment As Integer
            Get
                Return _PrimitiveData(1, 1)
            End Get
            Set(ByVal value As Integer)
                _PrimitiveData(1, 1) = value
                RefreshPrimitives()
            End Set
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Position of the segment defined as last primitive.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public Property LastPrimitiveSegment As Integer
            Get
                Return _PrimitiveData(1, 2)
            End Get
            Set(ByVal value As Integer)
                Me._PrimitiveData(1, 2) = value
                Me.RefreshPrimitives()
            End Set
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Position of the node defined as first primitive (depends on the first primitive segment).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property FirstPrimitiveNode As Integer
            Get
                Return Me._PrimitiveData(2, 1)
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Position of the node defined as last primitive (depends on the last primitive segment).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property LastPrimitiveNode As Integer
            Get
                Return Me._PrimitiveData(2, 2)
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Returns the index of a given primitive node.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property GetPrimitiveNodeIndex(ByVal Node As Integer) As Integer
            Get
                If Node >= 1 And Node <= Me._BoundaryNodesCount Then
                    Return Me._BoundaryNodes(Node)
                Else
                    Return 1
                End If
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Returns the location of a given primitive node.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property GetPrimitiveNodePosition(ByVal Nodo As Integer) As Vector3
            Get
                If Nodo >= 1 And Nodo <= Me._BoundaryNodesCount Then
                    Return Mesh.Nodes(_BoundaryNodes(Nodo)).Position
                Else
                    Return New Vector3
                End If
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Returns the index of a given primitive segment.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property GetPrimitivePanelIndex(ByVal Segment As Integer) As Integer
            Get
                If Segment >= 1 And Segment <= Me._BoundarySegmentsCount Then
                    Return _BoundaryPanels(Segment)
                Else
                    Return 1
                End If
            End Get
        End Property
        '------------------------------------------------------------------------------------------


        
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Returns the panel associated to a given primitive segment.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property GetPrimitivePanel(ByVal Segmento As Integer) As Panel
            Get
                If Segmento >= 1 And Segmento <= Me._BoundarySegmentsCount Then
                    Return Mesh.Panels(Me._BoundaryPanels(Segmento) - 1)
                Else
                    Return Mesh.Panels(0)
                End If
            End Get
        End Property
        '------------------------------------------------------------------------------------------

#End Region

#Region " Wing regions management "

        '==========================================================================================
        ' Recalculates the limits of the current region
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Loads a simple wing region.
        '==========================================================================================
        Public Sub InitializeRegions()

            Dim NewRegion As New WingRegion
            NewRegion.SpanPanelsCount = 5
            NewRegion.TipChord = 1.0#
            NewRegion.Length = 1.0#
            NewRegion.TwistAxis = 0.5#
            WingRegions.Add(NewRegion)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Adds a new region at the end.
        '==========================================================================================
        Public Sub AddRegion()

            Dim NewRegion As New WingRegion
            NewRegion.SpanPanelsCount = 5
            NewRegion.TipChord = 1.0#
            NewRegion.Length = 1.0#
            NewRegion.TwistAxis = 0.5#
            WingRegions.Add(NewRegion)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Insets a new wing region at a given position.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Inserts a new wing region after the current one.
        '==========================================================================================
        Public Overloads Sub InsertRegion()

            Dim NewRegion As New WingRegion
            NewRegion.SpanPanelsCount = 5
            NewRegion.TipChord = 1.0#
            NewRegion.Length = 1.0#
            NewRegion.TwistAxis = 0.5#
            WingRegions.Insert(_CurrentWingRegion, NewRegion)

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Removes the current wing region.
        '==========================================================================================
        Public Sub RemoveCurrentRegion()

            If WingRegions.Count > 1 Then

                WingRegions.RemoveAt(_CurrentWingRegion)
                _CurrentWingRegion = Math.Max(0, _CurrentWingRegion - 1)

            End If

        End Sub
        '------------------------------------------------------------------------------------------



        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Currently active region
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Public ReadOnly Property CurrentRegion As WingRegion
            Get
                Return WingRegions.Item(_CurrentWingRegion)
            End Get
        End Property
        '------------------------------------------------------------------------------------------

#End Region

#Region " 3D model and vortices generation "

        '==========================================================================================
        ' Generates the lifting surface mesh based on the geometrical parameters.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Refreshes the primitive node and panel indices.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------

#End Region

#Region " Structure "

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Determines whether or not the structure will be included on the calculation.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property IncludeStructure As Boolean = True

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Wing root section.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public Property RootSection As Section = New Section

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Contains all nodes and segments representing the structure.
        '//////////////////////////////////////////////////////////////////////////////////////////
        Private _StructuralPartition As New List(Of StructuralPartition)

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Represents the structural nodal points
        '//////////////////////////////////////////////////////////////////////////////////////////
        Public ReadOnly Property StructuralPartition As List(Of StructuralPartition)
            Get
                Return _StructuralPartition
            End Get
        End Property
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Generates the nodal partition and the section partition.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------

#End Region

#Region " UVLM method "

        '==========================================================================================
        ' Calculates the normal vectors and the control points.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------

#End Region

#Region " Other methods "

        '==========================================================================================
        ' Generates a copy of the instance.
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------

#End Region

#Region " Geometrical opperations "

        '==========================================================================================
        ' Aligns the surface by means of four reference points (it is not working well).
        '==========================================================================================
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
        '------------------------------------------------------------------------------------------

#End Region

#Region " IO "

        '==========================================================================================
        ' Reads the wing from an XML file.
        '==========================================================================================
        Public Overrides Sub ReadFromXML(ByRef Stream As XmlReader)

            Dim count As Integer = 1

            While Stream.Read

                If Not Stream.NodeType = XmlNodeType.Element Then Continue While

                Select Case Stream.Name

                    Case "Identity"

                        Name = Stream.GetAttribute("Name")
                        Id = New Guid(IOXML.ReadString(Stream, "ID", Guid.NewGuid.ToString))
                        IncludeInCalculation = IOXML.ReadBoolean(Stream, "Include", True)

                    Case "SurfaceProperties"

                        RootChord = IOXML.ReadDouble(Stream, "RootChord", 0.0)
                        RootFlap = IOXML.ReadDouble(Stream, "RootFlap", 0.2)
                        FlapPanels = IOXML.ReadInteger(Stream, "FlapPanels", 3)

                        Position.X = IOXML.ReadDouble(Stream, "X", 0.0)
                        Position.Y = IOXML.ReadDouble(Stream, "Y", 0.0)
                        Position.Z = IOXML.ReadDouble(Stream, "Z", 0.0)

                        Orientation.Angle1 = IOXML.ReadDouble(Stream, "Psi", 0.0)
                        Orientation.Angle2 = IOXML.ReadDouble(Stream, "Tita", 0.0)
                        Orientation.Angle3 = IOXML.ReadDouble(Stream, "Fi", 0.0)
                        Orientation.Sequence = IOXML.ReadInteger(Stream, "Sequence", CInt(RotationSequence.ZYX))

                        CenterOfRotation.X = IOXML.ReadDouble(Stream, "Xcr", 0.0)
                        CenterOfRotation.Y = IOXML.ReadDouble(Stream, "Ycr", 0.0)
                        CenterOfRotation.Z = IOXML.ReadDouble(Stream, "Zcr", 0.0)

                        FirstPrimitiveSegment = IOXML.ReadInteger(Stream, "Primitive1", 1)
                        LastPrimitiveSegment = IOXML.ReadInteger(Stream, "Primitive2", 2)
                        TrailingEdgeConvection = IOXML.ReadBoolean(Stream, "TrailingConvection", False)

                        ConvectWake = IOXML.ReadBoolean(Stream, "ConvectWake", True)
                        CuttingStep = IOXML.ReadInteger(Stream, "CuttingStep", 1)
                        Symmetric = IOXML.ReadBoolean(Stream, "Symmetric", True)

                        RootSection.AE = IOXML.ReadDouble(Stream, "RootA", 0.0)
                        RootSection.GJ = IOXML.ReadDouble(Stream, "RootIu", 0.0)
                        RootSection.EIy = IOXML.ReadDouble(Stream, "RootIv", 0.0)
                        RootSection.EIz = IOXML.ReadDouble(Stream, "RootIw", 0.0)
                        RootSection.Cmy = IOXML.ReadDouble(Stream, "RootSv", 0.0)
                        RootSection.Cmz = IOXML.ReadDouble(Stream, "RootSw", 0.0)
                        RootSection.Ip = IOXML.ReadDouble(Stream, "RootJ", 0.0)
                        RootSection.M = IOXML.ReadDouble(Stream, "Rootm", 0.0)

                        NumberOfChordPanels = IOXML.ReadInteger(Stream, "NumberChordRings", 6)

                    Case "MacroPanel", String.Format("MacroPanel{0}", count)

                        If count > 1 Then AddRegion()
                        CurrentRegionId = count
                        CurrentRegion.ReadFromXML(Stream)
                        count += 1

                    Case "VisualProperties"

                        VisualProperties.ReadFromXML(Stream.ReadSubtree)

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

            RefreshPrimitives()

            GenerateMesh()

        End Sub
        '------------------------------------------------------------------------------------------



        '==========================================================================================
        ' Writes the wing to an XML file.
        '==========================================================================================
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

            Stream.WriteAttributeString("RootChord", String.Format("{0}", RootChord))
            Stream.WriteAttributeString("RootFlap", String.Format("{0}", RootFlap))
            Stream.WriteAttributeString("FlapPanels", String.Format("{0}", FlapPanels))

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

            Stream.WriteAttributeString("Primitive1", String.Format("{0}", FirstPrimitiveSegment))
            Stream.WriteAttributeString("Primitive2", String.Format("{0}", LastPrimitiveSegment))
            Stream.WriteAttributeString("ConvectWake", String.Format("{0}", CInt(ConvectWake)))
            Stream.WriteAttributeString("CuttingStep", String.Format("{0}", CuttingStep))
            Stream.WriteAttributeString("Symmetric", String.Format("{0}", CInt(Symmetric)))
            Stream.WriteAttributeString("TrailingConvection", String.Format("{0}", CInt(TrailingEdgeConvection)))

            Stream.WriteAttributeString("RootA", String.Format("{0}", RootSection.AE))
            Stream.WriteAttributeString("RootIu", String.Format("{0}", RootSection.GJ))
            Stream.WriteAttributeString("RootIv", String.Format("{0}", RootSection.EIy))
            Stream.WriteAttributeString("RootIw", String.Format("{0}", RootSection.EIz))
            Stream.WriteAttributeString("RootSv", String.Format("{0}", RootSection.Cmy))
            Stream.WriteAttributeString("RootSw", String.Format("{0}", RootSection.Cmz))
            Stream.WriteAttributeString("RootJ", String.Format("{0}", RootSection.Ip))
            Stream.WriteAttributeString("Rootm", String.Format("{0}", RootSection.M))

            Stream.WriteAttributeString("NumberChordRings", String.Format("{0}", NumberOfChordPanels))
            Stream.WriteAttributeString("NumberMacroPanels", String.Format("{0}", WingRegions.Count))

            Stream.WriteEndElement()

            ' Macro panels
            '-----------------------------------------------------

            For i = 0 To WingRegions.Count - 1

                CurrentRegionId = i

                Stream.WriteStartElement("MacroPanel")
                WingRegions(i).WriteToXML(Stream)
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

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Writes a ASCII STL file containing the model mesh
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <param name="FilePath"></param>
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
        '------------------------------------------------------------------------------------------

        '//////////////////////////////////////////////////////////////////////////////////////////
        ' Returns a string with information about this wing.
        '//////////////////////////////////////////////////////////////////////////////////////////
        ' <returns></returns>
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

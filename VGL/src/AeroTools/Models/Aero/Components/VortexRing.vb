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

'' VGL dependencies
'-----------------------------------------------------------------------------
Imports VGL.MathTools.Algebra.EuclideanSpace

'#############################################################################
' Unit: VortexRing
'
' This unit provides a generic definition of a vortex ring.
' Vortex rings are used as basic blocks on lattices.
'#############################################################################
Namespace AeroTools.Models.Aero.Components

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' The two types of vortex rings
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Enum VortexRingType As Byte

        VR4 = 0
        VR3 = 1

    End Enum

    '//////////////////////////////////////////////////////////////////////////////////////////
    ' Represents a vortex ring
    '//////////////////////////////////////////////////////////////////////////////////////////
    Public Interface VortexRing

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' The type of vortex ring.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property Type As VortexRingType

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Total velocity at the control point.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property VelocityT As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' This is the velocity induced by the wakes. 
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property VelocityW As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Surface velocity at the control point.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property VelocityS As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Potential induced by wake doublets.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property PotentialW As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local circulation.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property G As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' First derivative of circulation in time.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property DGdt As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local source intensity.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property S As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local pressure coeficient. When parent lattice "IsSlender" field is true it 
        ' represents the coeficient of local jump of pressure.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property Cp As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local component of induced drag (only valid for slender rings).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property Cdi As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Local index: represents the position of this panel on the local storage.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property IndexL As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Global index: represents the index of this panel on the influence matrix and G and RHS vectors.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property IndexG As Integer

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Sets or gets the i-th corner node. This property is 1-based.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property Node(ByVal Index As Integer) As Node

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Normal vector at the control point.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property Normal As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Control point used to impose local boundary conditions.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property ControlPoint As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Control point used to impose local boundary conditions.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property OuterControlPoint As Vector3

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Ring area.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property Area As Double

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates if the sense of the ring is reversed.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property Reversed As Boolean

        '==========================================================================================
        ' Calculates all geometric entities associated to the vortex ring.
        '==========================================================================================
        Sub CalculateGeometricEntities()

        '==========================================================================================
        ' Forces the recalculation of the local coordinates.
        '==========================================================================================
        Sub RecalculateLocalCoordinates()

        '==========================================================================================
        ' Forces the recalculation of the normal.
        '==========================================================================================
        Sub RecalculateBasis()

        '==========================================================================================
        ' Returns the influence of the doublet distribution in the velocity.
        ' If WidthG is true vector is scaled by G.
        '==========================================================================================
        Function GetDoubletVelocityInfluence(ByVal Point As Vector3, Optional ByVal CutOff As Double = 0.0001, Optional ByVal WithG As Boolean = True) As Vector3

        '==========================================================================================
        ' Adds the influence of the doublet distribution in the velocity.
        ' If WidthG is true vector is scaled by G.
        '==========================================================================================
        Sub AddDoubletVelocityInfluence(ByRef Vector As Vector3,
                                        ByVal Point As Vector3,
                                        Optional ByVal CutOff As Double = 0.0001,
                                        Optional ByVal WithG As Boolean = True)

        '==========================================================================================
        ' Adds the influence of the source distribution in the velocity.
        '==========================================================================================
        Sub AddSourceVelocityInfluence(ByRef Vector As Vector3,
                                       ByVal Point As Vector3,
                                       Optional ByVal WithS As Boolean = True)

        '==========================================================================================
        ' Returns the influence of the doublet distribution in the velocity potential.
        '==========================================================================================
        Function GetDoubletPotentialInfluence(ByVal Point As Vector3, Optional ByVal WithG As Boolean = True) As Double

        '==========================================================================================
        ' Returns the influence of the source distribution in the velocity potential.
        '==========================================================================================
        Function GetSourcePotentialInfluence(ByVal Point As Vector3, Optional ByVal WithS As Boolean = True) As Double

        '==========================================================================================
        ' Computes the induced velocity at a given point by counting only the streamwise vortices.
        ' Point is where the influence is to be calculated.
        ' S1 is the first streamwise segment.
        ' S2 is the second streamwise segment.
        '==========================================================================================
        Function StreamwiseInfluence(ByVal Point As Vector3, ByVal S1 As Integer, ByVal S2 As Integer, Optional ByVal CutOff As Double = 0.0001) As Vector3

        '==========================================================================================
        ' Approaches the local velocity using the local source strength and the circulation of the adjacent panels.
        '==========================================================================================
        Sub CalculateLocalVelocity(StreamVelocity As Vector3)

        '==========================================================================================
        ' Calculates the jump of pressure through the vortex ring.
        ' VSqr os the square of reference velocity Norm2.
        '==========================================================================================
        Sub CalculateCp(ByVal VSqr As Double)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates whether the panel is used to convect wake or not. 
        ' This conditionates the local circulation to meet the Kutta condition.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property IsPrimitive As Boolean

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates whether the surface is slender or not. This will conditionate the calculation of the local pressure coeficient.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property IsSlender As Boolean

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Provides refference to the local neighbor rings. This field is driven by 
        ' "FindSurroundingRings" of parent lattice.
        '  If there is no ring at the given position "nothing" will be returned.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property SurroundingRing(ByVal side As UInt16, ByVal location As UInt16) As VortexRing

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Sence of the adjacent ring. 1 if same as this ring, -1 if oposite and 0 if there is
        ' no assigned ring on that location.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property SurroundingRingsSence(ByVal side As UInt16, ByVal location As UInt16) As Int16

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates whether this ring has a neighbor ring at the given position or not.
        ' 1-based index indicating the local position of the boundary line.
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ReadOnly Property HasNeighborAt(ByVal side As UInt16, ByVal location As UInt16)

        '==========================================================================================
        ' Attaches a neighbour ring at the following location on the given side.
        ' Side is a 0-based index indicating the local position of the boundary line.
        '==========================================================================================
        Sub AttachNeighbourAtSide(ByVal Side As Int16, ByRef Ring As VortexRing, ByVal Sence As Int16)

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Indicates the local circulation sence (1 or -1).
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property CirculationSence As Int16

        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ' Stores the circulation of each vortex segment composing this ring. Driven by 
        ' "CalculateGammas".
        '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Property Gamma(ByVal index As Integer) As Double

        '==========================================================================================
        ' Sets surrounding rings to nothing.
        '==========================================================================================
        Sub InitializeSurroundingRings()

    End Interface
    '----------------------------------------------------------------------------------------------

End Namespace
'--------------------------------------------------------------------------------------------------
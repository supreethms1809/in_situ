












! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!***********************************************************************
!
!  mpas_grid_types
!
!> \brief   MPAS Grid and field type defintion module
!> \author  Michael Duda, Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This module defines derived data types related to fields, and variable structures.
!> It also includes routines for allocating and deallocating these types.
!
!-----------------------------------------------------------------------
module mpas_grid_types

   use mpas_kind_types
   use mpas_dmpar_types
   use mpas_attlist
   use mpas_packages

   integer, parameter :: nTimeLevs = 2

   ! Derived type describing info for doing I/O specific to a field
   type io_info
      character (len=StrKIND) :: fieldName
      character (len=StrKIND) :: units
      character (len=StrKIND) :: description
      integer, dimension(4) :: start
      integer, dimension(4) :: count
      logical :: input
      logical :: sfc
      logical :: restart
      logical :: output
   end type io_info

   ! Derived type for storing fields
   type field5DReal
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:,:,:,:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(5) :: dimNames
      integer, dimension(5) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field5DReal), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field5DReal


   ! Derived type for storing fields
   type field4DReal
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:,:,:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(4) :: dimNames
      integer, dimension(4) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field4DReal), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field4DReal



   ! Derived type for storing fields
   type field3DReal
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:,:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(3) :: dimNames
      integer, dimension(3) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field3DReal), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field3DReal


   ! Derived type for storing fields
   type field2DReal
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(2) :: dimNames
      integer, dimension(2) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field2DReal), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field2DReal


   ! Derived type for storing fields
   type field1DReal
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(1) :: dimNames
      integer, dimension(1) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field1DReal), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field1DReal


   ! Derived type for storing fields
   type field0DReal
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      real (kind=RKIND) :: scalar

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field0DReal), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field0DReal


   ! Derived type for storing fields
   type field3DInteger
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      integer, dimension(:,:,:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(3) :: dimNames
      integer, dimension(3) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field3DInteger), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field3DInteger


   ! Derived type for storing fields
   type field2DInteger
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      integer, dimension(:,:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(2) :: dimNames
      integer, dimension(2) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field2DInteger), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field2DInteger


   ! Derived type for storing fields
   type field1DInteger
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      integer, dimension(:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(1) :: dimNames
      integer, dimension(1) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field1DInteger), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field1DInteger


   ! Derived type for storing fields
   type field0DInteger
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      integer :: scalar

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field0DInteger), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field0DInteger


   ! Derived type for storing fields
   type field1DChar
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      character (len=StrKIND), dimension(:), pointer :: array

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      character (len=StrKIND), dimension(1) :: dimNames
      integer, dimension(1) :: dimSizes
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      logical :: isPersistent
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field1DChar), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field1DChar


   ! Derived type for storing fields
   type field0DChar
  
      ! Back-pointer to the containing block
      type (block_type), pointer :: block

      ! Raw array holding field data on this block
      character (len=StrKIND) :: scalar

      ! Information used by the I/O layer
      type (io_info), pointer :: ioinfo       ! to be removed later
      character (len=StrKIND) :: fieldName
      character (len=StrKIND), dimension(:), pointer :: constituentNames => null()
      logical :: hasTimeDimension
      logical :: isActive
      logical :: isVarArray
      type (att_list_type), pointer :: attList => null()     

      ! Pointers to the prev and next blocks for this field on this task
      type (field0DChar), pointer :: prev, next

      ! Halo communication lists
      type (mpas_multihalo_exchange_list), pointer :: sendList
      type (mpas_multihalo_exchange_list), pointer :: recvList
      type (mpas_multihalo_exchange_list), pointer :: copyList
   end type field0DChar


   ! Derived type for storing grid meta-data
   type mesh_type

      type (block_type), pointer :: block

      integer :: nCells
      integer :: nEdges
      integer :: maxEdges
      integer :: maxEdges2
      integer :: nVertices
      integer :: vertexDegree
      integer :: nVertLevels
      integer :: nCellsSolve
      integer, dimension(:), pointer :: nCellsArray
      integer :: nEdgesSolve
      integer, dimension(:), pointer :: nEdgesArray
      integer :: maxEdgesSolve
      integer, dimension(:), pointer :: maxEdgesArray
      integer :: maxEdges2Solve
      integer, dimension(:), pointer :: maxEdges2Array
      integer :: nVerticesSolve
      integer, dimension(:), pointer :: nVerticesArray
      integer :: vertexDegreeSolve
      integer, dimension(:), pointer :: vertexDegreeArray
      integer :: nVertLevelsSolve
      integer, dimension(:), pointer :: nVertLevelsArray

      logical :: on_a_sphere
      real (kind=RKIND) :: sphere_radius

      type (field1dReal), pointer :: latCell
      type (field1dReal), pointer :: lonCell
      type (field1dReal), pointer :: xCell
      type (field1dReal), pointer :: yCell
      type (field1dReal), pointer :: zCell
      type (field1dInteger), pointer :: indexToCellID
      type (field1dReal), pointer :: latEdge
      type (field1dReal), pointer :: lonEdge
      type (field1dReal), pointer :: xEdge
      type (field1dReal), pointer :: yEdge
      type (field1dReal), pointer :: zEdge
      type (field1dInteger), pointer :: indexToEdgeID
      type (field1dReal), pointer :: latVertex
      type (field1dReal), pointer :: lonVertex
      type (field1dReal), pointer :: xVertex
      type (field1dReal), pointer :: yVertex
      type (field1dReal), pointer :: zVertex
      type (field1dInteger), pointer :: indexToVertexID
      type (field1dReal), pointer :: meshDensity
      type (field1dReal), pointer :: meshScalingDel2
      type (field1dReal), pointer :: meshScalingDel4
      type (field1dReal), pointer :: meshScaling
      type (field2dInteger), pointer :: cellsOnEdge
      type (field1dInteger), pointer :: nEdgesOnCell
      type (field1dInteger), pointer :: nEdgesOnEdge
      type (field2dInteger), pointer :: edgesOnCell
      type (field2dInteger), pointer :: edgesOnEdge
      type (field2dReal), pointer :: weightsOnEdge
      type (field1dReal), pointer :: dvEdge
      type (field1dReal), pointer :: dcEdge
      type (field1dReal), pointer :: angleEdge
      type (field1dReal), pointer :: areaCell
      type (field1dReal), pointer :: areaTriangle
      type (field2dReal), pointer :: edgeNormalVectors
      type (field2dReal), pointer :: edgeTangentVectors
      type (field2dReal), pointer :: localVerticalUnitVectors
      type (field3dReal), pointer :: cellTangentPlane
      type (field2dInteger), pointer :: cellsOnCell
      type (field2dInteger), pointer :: verticesOnCell
      type (field2dInteger), pointer :: verticesOnEdge
      type (field2dInteger), pointer :: edgesOnVertex
      type (field2dInteger), pointer :: cellsOnVertex
      type (field2dReal), pointer :: kiteAreasOnVertex
      type (field1dReal), pointer :: fEdge
      type (field1dReal), pointer :: fVertex
      type (field1dReal), pointer :: fCell
      type (field1dReal), pointer :: bottomDepth
      type (field3dReal), pointer :: derivTwo
      type (field2dReal), pointer :: advCoefs
      type (field2dReal), pointer :: advCoefs3rd
      type (field2dInteger), pointer :: advCellsForEdge
      type (field1dInteger), pointer :: nAdvCellsForEdge
      type (field2dInteger), pointer :: highOrderAdvectionMask
      type (field3dReal), pointer :: coeffs_reconstruct
      type (field1dInteger), pointer :: maxLevelCell
      type (field1dInteger), pointer :: maxLevelEdgeTop
      type (field1dInteger), pointer :: maxLevelEdgeBot
      type (field1dInteger), pointer :: maxLevelVertexTop
      type (field1dInteger), pointer :: maxLevelVertexBot
      type (field1dReal), pointer :: refBottomDepth
      type (field1dReal), pointer :: refBottomDepthTopOfCell
      type (field1dReal), pointer :: vertCoordMovementWeights
      type (field2dInteger), pointer :: boundaryEdge
      type (field2dInteger), pointer :: boundaryVertex
      type (field2dInteger), pointer :: boundaryCell
      type (field2dInteger), pointer :: edgeMask
      type (field2dInteger), pointer :: vertexMask
      type (field2dInteger), pointer :: cellMask
      type (field1dReal), pointer :: temperatureRestore
      type (field1dReal), pointer :: salinityRestore
      type (field2dInteger), pointer :: edgeSignOnCell
      type (field2dInteger), pointer :: edgeSignOnVertex
      type (field2dInteger), pointer :: kiteIndexOnCell

   end type mesh_type


   type state_type
      type (block_type), pointer :: block
      integer :: index_temperature = -1
      integer :: index_salinity = -1
      integer :: index_tracer1 = -1
      integer :: dynamics_start = -1
      integer :: dynamics_end = -1
      integer :: num_tracers = -1
      type (field3dReal), pointer :: tracers
      type (field2dReal), pointer :: normalVelocity
      type (field2dReal), pointer :: layerThickness
      type (field1dReal), pointer :: ssh
      type (field2dReal), pointer :: highFreqThickness
      type (field2dReal), pointer :: lowFreqDivergence
      type (field1dReal), pointer :: normalBarotropicVelocity
      type (field1dReal), pointer :: normalBarotropicVelocitySubcycle
      type (field1dReal), pointer :: sshSubcycle
      type (field2dReal), pointer :: normalBaroclinicVelocity
   end type state_type


   type state_pointer_type
      type (state_type), pointer :: state 
   end type state_pointer_type


   type state_multilevel_type
      integer :: nTimeLevels
      type (state_pointer_type), dimension(:), pointer :: time_levs
   end type state_multilevel_type


   type verticalMesh_type
      type (block_type), pointer :: block
      type (field2dReal), pointer :: restingThickness
      type (field1dReal), pointer :: refZMid
      type (field1dReal), pointer :: refLayerThickness
   end type verticalMesh_type


   type tend_type
      type (block_type), pointer :: block
      integer :: index_temperature = -1
      integer :: index_salinity = -1
      integer :: index_tracer1 = -1
      integer :: dynamics_start = -1
      integer :: dynamics_end = -1
      integer :: num_tracers = -1
      type (field3dReal), pointer :: tracers
      type (field2dReal), pointer :: normalVelocity
      type (field2dReal), pointer :: layerThickness
      type (field1dReal), pointer :: ssh
      type (field2dReal), pointer :: highFreqThickness
      type (field2dReal), pointer :: lowFreqDivergence
   end type tend_type


   type diagnostics_type
      type (block_type), pointer :: block
      integer :: index_temperatureSurfaceValue = -1
      integer :: index_salinitySurfaceValue = -1
      integer :: index_tracer1SurfaceValue = -1
      integer :: index_zonalSurfaceVelocity = -1
      integer :: index_meridionalSurfaceVelocity = -1
      integer :: index_zonalSSHGradient = -1
      integer :: index_meridionalSSHGradient = -1
      integer :: surfaceValues_start = -1
      integer :: surfaceValues_end = -1
      integer :: num_tracersSurfaceValue = -1
      integer :: vel_zonal_start = -1
      integer :: vel_zonal_end = -1
      integer :: vel_meridional_start = -1
      integer :: vel_meridional_end = -1
      integer :: num_surfaceVelocity = -1
      integer :: ssh_zonal_start = -1
      integer :: ssh_zonal_end = -1
      integer :: ssh_meridional_start = -1
      integer :: ssh_meridional_end = -1
      integer :: num_SSHGradient = -1
      type (field2dReal), pointer :: tracersSurfaceValue
      type (field2dReal), pointer :: surfaceVelocity
      type (field2dReal), pointer :: SSHGradient
      type (field0dChar), pointer :: xtime
      type (field2dReal), pointer :: zMid
      type (field2dReal), pointer :: zTop
      type (field2dReal), pointer :: density
      type (field2dReal), pointer :: displacedDensity
      type (field2dReal), pointer :: potentialDensity
      type (field2dReal), pointer :: BruntVaisalaFreqTop
      type (field2dReal), pointer :: montgomeryPotential
      type (field2dReal), pointer :: pressure
      type (field2dReal), pointer :: uTransport
      type (field2dReal), pointer :: vertTransportVelocityTop
      type (field2dReal), pointer :: vertVelocityTop
      type (field2dReal), pointer :: tangentialVelocity
      type (field2dReal), pointer :: layerThicknessEdge
      type (field2dReal), pointer :: layerThicknessVertex
      type (field2dReal), pointer :: kineticEnergyCell
      type (field2dReal), pointer :: hEddyFlux
      type (field2dReal), pointer :: hKappa
      type (field2dReal), pointer :: hKappaQ
      type (field2dReal), pointer :: viscosity
      type (field2dReal), pointer :: divergence
      type (field2dReal), pointer :: circulation
      type (field2dReal), pointer :: relativeVorticity
      type (field2dReal), pointer :: relativeVorticityCell
      type (field2dReal), pointer :: normalizedRelativeVorticityEdge
      type (field2dReal), pointer :: normalizedPlanetaryVorticityEdge
      type (field2dReal), pointer :: normalizedRelativeVorticityCell
      type (field1dReal), pointer :: barotropicForcing
      type (field1dReal), pointer :: barotropicThicknessFlux
      type (field2dReal), pointer :: normalVelocityX
      type (field2dReal), pointer :: normalVelocityY
      type (field2dReal), pointer :: normalVelocityZ
      type (field2dReal), pointer :: normalVelocityZonal
      type (field2dReal), pointer :: normalVelocityMeridional
      type (field2dReal), pointer :: gradSSH
      type (field2dReal), pointer :: gradSSHX
      type (field2dReal), pointer :: gradSSHY
      type (field2dReal), pointer :: gradSSHZ
      type (field2dReal), pointer :: gradSSHZonal
      type (field2dReal), pointer :: gradSSHMeridional
      type (field2dReal), pointer :: uBolusGM
      type (field2dReal), pointer :: uBolusGMX
      type (field2dReal), pointer :: uBolusGMY
      type (field2dReal), pointer :: uBolusGMZ
      type (field2dReal), pointer :: uBolusGMZonal
      type (field2dReal), pointer :: uBolusGMMeridional
      type (field2dReal), pointer :: RiTopOfCell
      type (field2dReal), pointer :: RiTopOfEdge
      type (field2dReal), pointer :: vertViscTopOfEdge
      type (field2dReal), pointer :: vertViscTopOfCell
      type (field2dReal), pointer :: vertDiffTopOfCell
      type (field2dReal), pointer :: bulkRichardsonNumber
      type (field1dReal), pointer :: boundaryLayerDepth
      type (field1dReal), pointer :: indexBoundaryLayerDepth
      type (field1dReal), pointer :: surfaceFrictionVelocity
      type (field1dReal), pointer :: windStressZonalDiag
      type (field1dReal), pointer :: windStressMeridionalDiag
      type (field1dReal), pointer :: penetrativeTemperatureFluxOBL
      type (field1dReal), pointer :: buoyancyForcingOBL
      type (field0dReal), pointer :: areaCellGlobal
      type (field0dReal), pointer :: areaEdgeGlobal
      type (field0dReal), pointer :: areaTriangleGlobal
      type (field0dReal), pointer :: volumeCellGlobal
      type (field0dReal), pointer :: volumeEdgeGlobal
      type (field0dReal), pointer :: CFLNumberGlobal
   end type diagnostics_type


   type average_type
      type (block_type), pointer :: block
      type (field0dReal), pointer :: nAverage
      type (field1dReal), pointer :: avgSsh
      type (field1dReal), pointer :: varSsh
      type (field2dReal), pointer :: avgVelocityZonal
      type (field2dReal), pointer :: avgVelocityMeridional
      type (field2dReal), pointer :: varVelocityZonal
      type (field2dReal), pointer :: varVelocityMeridional
      type (field2dReal), pointer :: avgNormalVelocity
      type (field2dReal), pointer :: varNormalVelocity
      type (field2dReal), pointer :: avgVertVelocityTop
   end type average_type


   type forcing_type
      type (block_type), pointer :: block
      integer :: index_surfaceTemperatureFlux = -1
      integer :: index_surfaceSalinityFlux = -1
      integer :: index_surfaceTracer1Flux = -1
      integer :: index_temperatureSurfaceValue = -1
      integer :: index_salinitySurfaceValue = -1
      integer :: index_avgTracer1SurfaceValue = -1
      integer :: index_avgZonalSurfaceVelocity = -1
      integer :: index_avgMeridionalSurfaceVelocity = -1
      integer :: index_avgZonalSSHGradient = -1
      integer :: index_avgMeridionalSSHGradient = -1
      integer :: dynamics_start = -1
      integer :: dynamics_end = -1
      integer :: testing_start = -1
      integer :: testing_end = -1
      integer :: num_surfaceTracerFlux = -1
      integer :: surfaceValues_start = -1
      integer :: surfaceValues_end = -1
      integer :: num_avgTracersSurfaceValue = -1
      integer :: vel_zonal_start = -1
      integer :: vel_zonal_end = -1
      integer :: vel_meridional_start = -1
      integer :: vel_meridional_end = -1
      integer :: num_avgSurfaceVelocity = -1
      integer :: ssh_zonal_start = -1
      integer :: ssh_zonal_end = -1
      integer :: ssh_meridional_start = -1
      integer :: ssh_meridional_end = -1
      integer :: num_avgSSHGradient = -1
      type (field2dReal), pointer :: surfaceTracerFlux
      type (field2dReal), pointer :: avgTracersSurfaceValue
      type (field2dReal), pointer :: avgSurfaceVelocity
      type (field2dReal), pointer :: avgSSHGradient
      type (field1dReal), pointer :: surfaceWindStress
      type (field1dReal), pointer :: surfaceWindStressMagnitude
      type (field1dReal), pointer :: surfaceMassFlux
      type (field1dReal), pointer :: seaSurfacePressure
      type (field1dReal), pointer :: seaIceEnergy
      type (field1dReal), pointer :: penetrativeTemperatureFlux
      type (field2dReal), pointer :: transmissionCoefficients
      type (field1dReal), pointer :: windStressZonal
      type (field1dReal), pointer :: windStressMeridional
      type (field1dReal), pointer :: latentHeatFlux
      type (field1dReal), pointer :: sensibleHeatFlux
      type (field1dReal), pointer :: longWaveHeatFluxUp
      type (field1dReal), pointer :: longWaveHeatFluxDown
      type (field1dReal), pointer :: seaIceHeatFlux
      type (field1dReal), pointer :: shortWaveHeatFlux
      type (field1dReal), pointer :: evaporationFlux
      type (field1dReal), pointer :: seaIceSalinityFlux
      type (field1dReal), pointer :: seaIceFreshWaterFlux
      type (field1dReal), pointer :: riverRunoffFlux
      type (field1dReal), pointer :: iceRunoffFlux
      type (field1dReal), pointer :: rainFlux
      type (field1dReal), pointer :: snowFlux
      type (field1dReal), pointer :: iceFraction
      type (field1dReal), pointer :: prognosticCO2
      type (field1dReal), pointer :: diagnosticCO2
      type (field1dReal), pointer :: squaredWindSpeed10Meter
      type (field0dInteger), pointer :: nAccumulatedCoupled
      type (field1dReal), pointer :: CO2Flux
      type (field1dReal), pointer :: DMSFlux
   end type forcing_type


   type scratch_type
      type (block_type), pointer :: block
      type (field2dReal), pointer :: vorticityGradientTangentialComponent
      type (field2dReal), pointer :: vorticityGradientNormalComponent
      type (field2dReal), pointer :: normalizedRelativeVorticityVertex
      type (field2dReal), pointer :: normalizedPlanetaryVorticityVertex
      type (field2dReal), pointer :: kineticEnergyVertex
      type (field2dReal), pointer :: kineticEnergyVertexOnCells
      type (field2dReal), pointer :: densitySurfaceDisplaced
      type (field2dReal), pointer :: thermalExpansionCoeff
      type (field2dReal), pointer :: salineContractionCoeff
      type (field2dReal), pointer :: normalVelocityTest
      type (field2dReal), pointer :: tangentialVelocityTest
      type (field3dReal), pointer :: strainRateR3Cell
      type (field3dReal), pointer :: strainRateR3CellSolution
      type (field3dReal), pointer :: strainRateR3Edge
      type (field3dReal), pointer :: strainRateLonLatRCell
      type (field3dReal), pointer :: strainRateLonLatRCellSolution
      type (field3dReal), pointer :: strainRateLonLatREdge
      type (field3dReal), pointer :: divTensorR3Cell
      type (field3dReal), pointer :: divTensorR3CellSolution
      type (field3dReal), pointer :: divTensorLonLatRCell
      type (field3dReal), pointer :: divTensorLonLatRCellSolution
      type (field3dReal), pointer :: outerProductEdge
      type (field2dReal), pointer :: normalVectorEdge
      type (field2dReal), pointer :: tangentialVectorEdge
      type (field2dReal), pointer :: windStressFull
      type (field2dReal), pointer :: windStressX
      type (field2dReal), pointer :: windStressY
      type (field2dReal), pointer :: windStressZ
      type (field2dReal), pointer :: windStressZonal
      type (field2dReal), pointer :: windStressMeridional
   end type scratch_type




   ! Type for storing (possibly architecture specific) information concerning to parallelism
   type parallel_info
      type (mpas_multihalo_exchange_list), pointer :: cellsToSend            ! List of types describing which cells to send to other blocks
      type (mpas_multihalo_exchange_list), pointer :: cellsToRecv            ! List of types describing which cells to receive from other blocks
      type (mpas_multihalo_exchange_list), pointer :: cellsToCopy            ! List of types describing which cells to copy from other blocks

      type (mpas_multihalo_exchange_list), pointer :: edgesToSend            ! List of types describing which edges to send to other blocks
      type (mpas_multihalo_exchange_list), pointer :: edgesToRecv            ! List of types describing which edges to receive from other blocks
      type (mpas_multihalo_exchange_list), pointer :: edgesToCopy            ! List of types describing which edges to copy from other blocks

      type (mpas_multihalo_exchange_list), pointer :: verticesToSend         ! List of types describing which vertices to send to other blocks
      type (mpas_multihalo_exchange_list), pointer :: verticesToRecv         ! List of types describing which vertices to receive from other blocks
      type (mpas_multihalo_exchange_list), pointer :: verticesToCopy         ! List of types describing which vertices to copy from other blocks
   end type parallel_info


   ! Derived type for storing part of a domain; used as a basic unit of work for a process
   type block_type

      type (state_multilevel_type), pointer :: state
      type (state_type), pointer :: provis_state
      type (mesh_type), pointer :: mesh
      type (verticalMesh_type), pointer :: verticalMesh
      type (tend_type), pointer :: tend
      type (diagnostics_type), pointer :: diagnostics
      type (average_type), pointer :: average
      type (forcing_type), pointer :: forcing
      type (scratch_type), pointer :: scratch

      integer :: blockID   ! Unique global ID number for this block
      integer :: localBlockID  ! Unique local ID number for this block

      type (domain_type), pointer :: domain

      type (parallel_info), pointer :: parinfo

      type (block_type), pointer :: prev, next
   end type block_type


   ! Derived type for storing list of blocks from a domain to be handled by a process
   type domain_type
      type (block_type), pointer :: blocklist
   
      ! Also store parallelization info here
      type (dm_info), pointer :: dminfo
       character (len=StrKIND) :: modelName = 'mpas' !< Constant: Name of model
       character (len=StrKIND) :: coreName = 'ocean' !< Constant: Name of core
       character (len=StrKIND) :: modelVersion = '2.0' !< Constant: Version number
      character (len=StrKIND*2) :: history !< History attribute, read in from input file.
   end type domain_type

   interface mpas_allocate_scratch_field
     module procedure mpas_allocate_scratch_field1d_integer
     module procedure mpas_allocate_scratch_field2d_integer
     module procedure mpas_allocate_scratch_field3d_integer
     module procedure mpas_allocate_scratch_field1d_real
     module procedure mpas_allocate_scratch_field2d_real
     module procedure mpas_allocate_scratch_field3d_real
     module procedure mpas_allocate_scratch_field4d_real
     module procedure mpas_allocate_scratch_field5d_real
     module procedure mpas_allocate_scratch_field1d_char
   end interface

   interface mpas_deallocate_scratch_field
     module procedure mpas_deallocate_scratch_field1d_integer
     module procedure mpas_deallocate_scratch_field2d_integer
     module procedure mpas_deallocate_scratch_field3d_integer
     module procedure mpas_deallocate_scratch_field1d_real
     module procedure mpas_deallocate_scratch_field2d_real
     module procedure mpas_deallocate_scratch_field3d_real
     module procedure mpas_deallocate_scratch_field4d_real
     module procedure mpas_deallocate_scratch_field5d_real
     module procedure mpas_deallocate_scratch_field1d_char
   end interface

   interface mpas_deallocate_field
     module procedure mpas_deallocate_field0d_integer
     module procedure mpas_deallocate_field1d_integer
     module procedure mpas_deallocate_field2d_integer
     module procedure mpas_deallocate_field3d_integer
     module procedure mpas_deallocate_field0d_real
     module procedure mpas_deallocate_field1d_real
     module procedure mpas_deallocate_field2d_real
     module procedure mpas_deallocate_field3d_real
     module procedure mpas_deallocate_field4d_real
     module procedure mpas_deallocate_field5d_real
     module procedure mpas_deallocate_field0d_char
     module procedure mpas_deallocate_field1d_char
   end interface


   contains

!***********************************************************************
!
!  routine mpas_allocate_domain
!
!> \brief   MPAS Domain allocation routine
!> \author  Michael Duda
!> \date    04/02/13
!> \details 
!> This routine allocates a domain structure.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_domain(dom, dminfo)!{{{

      implicit none

      type (domain_type), pointer :: dom !< Input/Output: Domain structure
      type (dm_info), pointer :: dminfo !< Input: Domain Information

      allocate(dom)
      nullify(dom % blocklist)
      dom % dminfo => dminfo

   end subroutine mpas_allocate_domain!}}}

!***********************************************************************
!
!  routine mpas_allocate_block
!
!> \brief   MPAS Block allocation routine
!> \author  Michael Duda
!> \date    04/02/13
!> \details 
!> This routine allocates a block structure. It calls routines to allocate the variable structures
!> that are members of the block type.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_block(nHaloLayers, b, dom, blockID, &!{{{
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                            )

      implicit none

      integer, intent(in) :: nHaloLayers !< Input: Number of halo laters
      type (block_type), pointer :: b !< Input/Output: Block structure
      type (domain_type), pointer :: dom !< Input: Domain structure
      integer, intent(in) :: blockID !< Input: Global ID of block
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels



      integer :: i

      b % blockID = blockID

      allocate(b % parinfo)

      b % domain => dom

      allocate(b % state)
      b % state % nTimeLevels = 2
      allocate(b % state % time_levs(2))
      do i=1,b % state % nTimeLevels
         allocate(b % state % time_levs(i) % state)
         call mpas_allocate_state(b, b % state % time_levs(i) % state, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )
      end do

      allocate(b % mesh)
      call mpas_allocate_mesh(b, b % mesh, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                      )

      allocate(b % verticalMesh)
      call mpas_allocate_verticalMesh(b, b % verticalMesh, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                      )

      allocate(b % tend)
      call mpas_allocate_tend(b, b % tend, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                      )

      allocate(b % diagnostics)
      call mpas_allocate_diagnostics(b, b % diagnostics, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                      )

      allocate(b % average)
      call mpas_allocate_average(b, b % average, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                      )

      allocate(b % forcing)
      call mpas_allocate_forcing(b, b % forcing, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                      )

      allocate(b % scratch)
      call mpas_allocate_scratch(b, b % scratch, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                      )


   end subroutine mpas_allocate_block!}}}


   subroutine mpas_allocate_state(b, state, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (state_type), intent(inout) :: state
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      state % block => b
      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(state % tracers)
      allocate(state % tracers % ioinfo)
      state % tracers % fieldName = 'tracers'
      state % tracers % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      state % index_temperature = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      state % index_salinity = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      state % index_tracer1 = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         state % dynamics_start = group_start
         state % dynamics_end = group_start + group_counter
      end if
      state % num_tracers = index_counter
      if ( state % num_tracers > 0 ) then
         allocate(state % tracers % constituentNames(state % num_tracers))
      end if
      state % tracers % constituentNames(state % index_temperature) = 'temperature'
      state % tracers % constituentNames(state % index_salinity) = 'salinity'
      state % tracers % constituentNames(state % index_tracer1) = 'tracer1'
      state % tracers % isPersistent = .true.
      state % tracers % isActive = .true.
      allocate(state % tracers % array(state % num_tracers, nVertLevels, nCells + 1))
      state % tracers % array = 0.0_RKIND
      state % tracers % dimSizes(1) = state % num_tracers
      state % tracers % dimNames(1) = 'num_tracers'
      state % tracers % dimSizes(2) = nVertLevels
      state % tracers % dimNames(2) = 'nVertLevels'
      state % tracers % dimSizes(3) = nCells
      state % tracers % dimNames(3) = 'nCells'
      state % tracers % hasTimeDimension = .true.
      nullify(state % tracers % prev)
      nullify(state % tracers % next)
      nullify(state % tracers % sendList)
      nullify(state % tracers % recvList)
      nullify(state % tracers % copyList)
      state % tracers % ioinfo % input = .true.
      state % tracers % ioinfo % sfc = .false.
      state % tracers % ioinfo % restart = .true.
      state % tracers % ioinfo % output = .true.
      state % tracers % block => b

      allocate(state % normalVelocity)
      allocate(state % normalVelocity % ioinfo)
      state % normalVelocity % fieldName = 'normalVelocity'
      state % normalVelocity % isVarArray = .false.
      state % normalVelocity % isPersistent = .true.
      state % normalVelocity % isActive = .true.
      allocate(state % normalVelocity % array(nVertLevels, nEdges + 1))
      state % normalVelocity % array = 0.0_RKIND
      state % normalVelocity % dimSizes(1) = nVertLevels
      state % normalVelocity % dimNames(1) = 'nVertLevels'
      state % normalVelocity % dimSizes(2) = nEdges
      state % normalVelocity % dimNames(2) = 'nEdges'
      state % normalVelocity % hasTimeDimension = .true.
      nullify(state % normalVelocity % prev)
      nullify(state % normalVelocity % next)
      nullify(state % normalVelocity % sendList)
      nullify(state % normalVelocity % recvList)
      nullify(state % normalVelocity % copyList)
      state % normalVelocity % ioinfo % input = .true.
      state % normalVelocity % ioinfo % sfc = .false.
      state % normalVelocity % ioinfo % restart = .true.
      state % normalVelocity % ioinfo % output = .false.
      state % normalVelocity % block => b

      allocate(state % layerThickness)
      allocate(state % layerThickness % ioinfo)
      state % layerThickness % fieldName = 'layerThickness'
      state % layerThickness % isVarArray = .false.
      state % layerThickness % isPersistent = .true.
      state % layerThickness % isActive = .true.
      allocate(state % layerThickness % array(nVertLevels, nCells + 1))
      state % layerThickness % array = 0.0_RKIND
      state % layerThickness % dimSizes(1) = nVertLevels
      state % layerThickness % dimNames(1) = 'nVertLevels'
      state % layerThickness % dimSizes(2) = nCells
      state % layerThickness % dimNames(2) = 'nCells'
      state % layerThickness % hasTimeDimension = .true.
      nullify(state % layerThickness % prev)
      nullify(state % layerThickness % next)
      nullify(state % layerThickness % sendList)
      nullify(state % layerThickness % recvList)
      nullify(state % layerThickness % copyList)
      state % layerThickness % ioinfo % input = .true.
      state % layerThickness % ioinfo % sfc = .false.
      state % layerThickness % ioinfo % restart = .true.
      state % layerThickness % ioinfo % output = .true.
      state % layerThickness % block => b

      allocate(state % ssh)
      allocate(state % ssh % ioinfo)
      state % ssh % fieldName = 'ssh'
      state % ssh % isVarArray = .false.
      state % ssh % isPersistent = .true.
      state % ssh % isActive = .true.
      allocate(state % ssh % array(nCells + 1))
      state % ssh % array = 0.0_RKIND
      state % ssh % dimSizes(1) = nCells
      state % ssh % dimNames(1) = 'nCells'
      state % ssh % hasTimeDimension = .true.
      nullify(state % ssh % prev)
      nullify(state % ssh % next)
      nullify(state % ssh % sendList)
      nullify(state % ssh % recvList)
      nullify(state % ssh % copyList)
      state % ssh % ioinfo % input = .false.
      state % ssh % ioinfo % sfc = .false.
      state % ssh % ioinfo % restart = .false.
      state % ssh % ioinfo % output = .true.
      state % ssh % block => b

      allocate(state % highFreqThickness)
      allocate(state % highFreqThickness % ioinfo)
      state % highFreqThickness % fieldName = 'highFreqThickness'
      state % highFreqThickness % isVarArray = .false.
      state % highFreqThickness % isPersistent = .true.
      state % highFreqThickness % isActive = .true.
      allocate(state % highFreqThickness % array(nVertLevels, nCells + 1))
      state % highFreqThickness % array = 0.0_RKIND
      state % highFreqThickness % dimSizes(1) = nVertLevels
      state % highFreqThickness % dimNames(1) = 'nVertLevels'
      state % highFreqThickness % dimSizes(2) = nCells
      state % highFreqThickness % dimNames(2) = 'nCells'
      state % highFreqThickness % hasTimeDimension = .true.
      nullify(state % highFreqThickness % prev)
      nullify(state % highFreqThickness % next)
      nullify(state % highFreqThickness % sendList)
      nullify(state % highFreqThickness % recvList)
      nullify(state % highFreqThickness % copyList)
      state % highFreqThickness % ioinfo % input = .true.
      state % highFreqThickness % ioinfo % sfc = .false.
      state % highFreqThickness % ioinfo % restart = .true.
      state % highFreqThickness % ioinfo % output = .false.
      state % highFreqThickness % block => b

      allocate(state % lowFreqDivergence)
      allocate(state % lowFreqDivergence % ioinfo)
      state % lowFreqDivergence % fieldName = 'lowFreqDivergence'
      state % lowFreqDivergence % isVarArray = .false.
      state % lowFreqDivergence % isPersistent = .true.
      state % lowFreqDivergence % isActive = .true.
      allocate(state % lowFreqDivergence % array(nVertLevels, nCells + 1))
      state % lowFreqDivergence % array = 0.0_RKIND
      state % lowFreqDivergence % dimSizes(1) = nVertLevels
      state % lowFreqDivergence % dimNames(1) = 'nVertLevels'
      state % lowFreqDivergence % dimSizes(2) = nCells
      state % lowFreqDivergence % dimNames(2) = 'nCells'
      state % lowFreqDivergence % hasTimeDimension = .true.
      nullify(state % lowFreqDivergence % prev)
      nullify(state % lowFreqDivergence % next)
      nullify(state % lowFreqDivergence % sendList)
      nullify(state % lowFreqDivergence % recvList)
      nullify(state % lowFreqDivergence % copyList)
      state % lowFreqDivergence % ioinfo % input = .true.
      state % lowFreqDivergence % ioinfo % sfc = .false.
      state % lowFreqDivergence % ioinfo % restart = .true.
      state % lowFreqDivergence % ioinfo % output = .false.
      state % lowFreqDivergence % block => b

      allocate(state % normalBarotropicVelocity)
      allocate(state % normalBarotropicVelocity % ioinfo)
      state % normalBarotropicVelocity % fieldName = 'normalBarotropicVelocity'
      state % normalBarotropicVelocity % isVarArray = .false.
      state % normalBarotropicVelocity % isPersistent = .true.
      state % normalBarotropicVelocity % isActive = .true.
      allocate(state % normalBarotropicVelocity % array(nEdges + 1))
      state % normalBarotropicVelocity % array = 0.0_RKIND
      state % normalBarotropicVelocity % dimSizes(1) = nEdges
      state % normalBarotropicVelocity % dimNames(1) = 'nEdges'
      state % normalBarotropicVelocity % hasTimeDimension = .true.
      nullify(state % normalBarotropicVelocity % prev)
      nullify(state % normalBarotropicVelocity % next)
      nullify(state % normalBarotropicVelocity % sendList)
      nullify(state % normalBarotropicVelocity % recvList)
      nullify(state % normalBarotropicVelocity % copyList)
      state % normalBarotropicVelocity % ioinfo % input = .false.
      state % normalBarotropicVelocity % ioinfo % sfc = .false.
      state % normalBarotropicVelocity % ioinfo % restart = .true.
      state % normalBarotropicVelocity % ioinfo % output = .false.
      state % normalBarotropicVelocity % block => b

      allocate(state % normalBarotropicVelocitySubcycle)
      allocate(state % normalBarotropicVelocitySubcycle % ioinfo)
      state % normalBarotropicVelocitySubcycle % fieldName = 'normalBarotropicVelocitySubcycle'
      state % normalBarotropicVelocitySubcycle % isVarArray = .false.
      state % normalBarotropicVelocitySubcycle % isPersistent = .true.
      state % normalBarotropicVelocitySubcycle % isActive = .true.
      allocate(state % normalBarotropicVelocitySubcycle % array(nEdges + 1))
      state % normalBarotropicVelocitySubcycle % array = 0.0_RKIND
      state % normalBarotropicVelocitySubcycle % dimSizes(1) = nEdges
      state % normalBarotropicVelocitySubcycle % dimNames(1) = 'nEdges'
      state % normalBarotropicVelocitySubcycle % hasTimeDimension = .true.
      nullify(state % normalBarotropicVelocitySubcycle % prev)
      nullify(state % normalBarotropicVelocitySubcycle % next)
      nullify(state % normalBarotropicVelocitySubcycle % sendList)
      nullify(state % normalBarotropicVelocitySubcycle % recvList)
      nullify(state % normalBarotropicVelocitySubcycle % copyList)
      state % normalBarotropicVelocitySubcycle % ioinfo % input = .false.
      state % normalBarotropicVelocitySubcycle % ioinfo % sfc = .false.
      state % normalBarotropicVelocitySubcycle % ioinfo % restart = .false.
      state % normalBarotropicVelocitySubcycle % ioinfo % output = .false.
      state % normalBarotropicVelocitySubcycle % block => b

      allocate(state % sshSubcycle)
      allocate(state % sshSubcycle % ioinfo)
      state % sshSubcycle % fieldName = 'sshSubcycle'
      state % sshSubcycle % isVarArray = .false.
      state % sshSubcycle % isPersistent = .true.
      state % sshSubcycle % isActive = .true.
      allocate(state % sshSubcycle % array(nCells + 1))
      state % sshSubcycle % array = 0.0_RKIND
      state % sshSubcycle % dimSizes(1) = nCells
      state % sshSubcycle % dimNames(1) = 'nCells'
      state % sshSubcycle % hasTimeDimension = .true.
      nullify(state % sshSubcycle % prev)
      nullify(state % sshSubcycle % next)
      nullify(state % sshSubcycle % sendList)
      nullify(state % sshSubcycle % recvList)
      nullify(state % sshSubcycle % copyList)
      state % sshSubcycle % ioinfo % input = .false.
      state % sshSubcycle % ioinfo % sfc = .false.
      state % sshSubcycle % ioinfo % restart = .false.
      state % sshSubcycle % ioinfo % output = .false.
      state % sshSubcycle % block => b

      allocate(state % normalBaroclinicVelocity)
      allocate(state % normalBaroclinicVelocity % ioinfo)
      state % normalBaroclinicVelocity % fieldName = 'normalBaroclinicVelocity'
      state % normalBaroclinicVelocity % isVarArray = .false.
      state % normalBaroclinicVelocity % isPersistent = .true.
      state % normalBaroclinicVelocity % isActive = .true.
      allocate(state % normalBaroclinicVelocity % array(nVertLevels, nEdges + 1))
      state % normalBaroclinicVelocity % array = 0.0_RKIND
      state % normalBaroclinicVelocity % dimSizes(1) = nVertLevels
      state % normalBaroclinicVelocity % dimNames(1) = 'nVertLevels'
      state % normalBaroclinicVelocity % dimSizes(2) = nEdges
      state % normalBaroclinicVelocity % dimNames(2) = 'nEdges'
      state % normalBaroclinicVelocity % hasTimeDimension = .true.
      nullify(state % normalBaroclinicVelocity % prev)
      nullify(state % normalBaroclinicVelocity % next)
      nullify(state % normalBaroclinicVelocity % sendList)
      nullify(state % normalBaroclinicVelocity % recvList)
      nullify(state % normalBaroclinicVelocity % copyList)
      state % normalBaroclinicVelocity % ioinfo % input = .false.
      state % normalBaroclinicVelocity % ioinfo % sfc = .false.
      state % normalBaroclinicVelocity % ioinfo % restart = .false.
      state % normalBaroclinicVelocity % ioinfo % output = .false.
      state % normalBaroclinicVelocity % block => b

   end subroutine mpas_allocate_state


   subroutine mpas_allocate_mesh(b, mesh, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (mesh_type), intent(inout) :: mesh
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      mesh % block => b
      mesh % nCells = nCells
      mesh % nEdges = nEdges
      mesh % maxEdges = maxEdges
      mesh % maxEdges2 = maxEdges2
      mesh % nVertices = nVertices
      mesh % vertexDegree = vertexDegree
      mesh % nVertLevels = nVertLevels

      allocate(mesh % latCell)
      allocate(mesh % latCell % ioinfo)
      mesh % latCell % fieldName = 'latCell'
      mesh % latCell % isVarArray = .false.
      mesh % latCell % isPersistent = .true.
      mesh % latCell % isActive = .true.
      allocate(mesh % latCell % array(nCells + 1))
      mesh % latCell % array = 0.0_RKIND
      mesh % latCell % dimSizes(1) = nCells
      mesh % latCell % dimNames(1) = 'nCells'
      mesh % latCell % hasTimeDimension = .false.
      nullify(mesh % latCell % prev)
      nullify(mesh % latCell % next)
      nullify(mesh % latCell % sendList)
      nullify(mesh % latCell % recvList)
      nullify(mesh % latCell % copyList)
      mesh % latCell % ioinfo % input = .true.
      mesh % latCell % ioinfo % sfc = .false.
      mesh % latCell % ioinfo % restart = .true.
      mesh % latCell % ioinfo % output = .true.
      mesh % latCell % block => b

      allocate(mesh % lonCell)
      allocate(mesh % lonCell % ioinfo)
      mesh % lonCell % fieldName = 'lonCell'
      mesh % lonCell % isVarArray = .false.
      mesh % lonCell % isPersistent = .true.
      mesh % lonCell % isActive = .true.
      allocate(mesh % lonCell % array(nCells + 1))
      mesh % lonCell % array = 0.0_RKIND
      mesh % lonCell % dimSizes(1) = nCells
      mesh % lonCell % dimNames(1) = 'nCells'
      mesh % lonCell % hasTimeDimension = .false.
      nullify(mesh % lonCell % prev)
      nullify(mesh % lonCell % next)
      nullify(mesh % lonCell % sendList)
      nullify(mesh % lonCell % recvList)
      nullify(mesh % lonCell % copyList)
      mesh % lonCell % ioinfo % input = .true.
      mesh % lonCell % ioinfo % sfc = .false.
      mesh % lonCell % ioinfo % restart = .true.
      mesh % lonCell % ioinfo % output = .true.
      mesh % lonCell % block => b

      allocate(mesh % xCell)
      allocate(mesh % xCell % ioinfo)
      mesh % xCell % fieldName = 'xCell'
      mesh % xCell % isVarArray = .false.
      mesh % xCell % isPersistent = .true.
      mesh % xCell % isActive = .true.
      allocate(mesh % xCell % array(nCells + 1))
      mesh % xCell % array = 0.0_RKIND
      mesh % xCell % dimSizes(1) = nCells
      mesh % xCell % dimNames(1) = 'nCells'
      mesh % xCell % hasTimeDimension = .false.
      nullify(mesh % xCell % prev)
      nullify(mesh % xCell % next)
      nullify(mesh % xCell % sendList)
      nullify(mesh % xCell % recvList)
      nullify(mesh % xCell % copyList)
      mesh % xCell % ioinfo % input = .true.
      mesh % xCell % ioinfo % sfc = .false.
      mesh % xCell % ioinfo % restart = .true.
      mesh % xCell % ioinfo % output = .true.
      mesh % xCell % block => b

      allocate(mesh % yCell)
      allocate(mesh % yCell % ioinfo)
      mesh % yCell % fieldName = 'yCell'
      mesh % yCell % isVarArray = .false.
      mesh % yCell % isPersistent = .true.
      mesh % yCell % isActive = .true.
      allocate(mesh % yCell % array(nCells + 1))
      mesh % yCell % array = 0.0_RKIND
      mesh % yCell % dimSizes(1) = nCells
      mesh % yCell % dimNames(1) = 'nCells'
      mesh % yCell % hasTimeDimension = .false.
      nullify(mesh % yCell % prev)
      nullify(mesh % yCell % next)
      nullify(mesh % yCell % sendList)
      nullify(mesh % yCell % recvList)
      nullify(mesh % yCell % copyList)
      mesh % yCell % ioinfo % input = .true.
      mesh % yCell % ioinfo % sfc = .false.
      mesh % yCell % ioinfo % restart = .true.
      mesh % yCell % ioinfo % output = .true.
      mesh % yCell % block => b

      allocate(mesh % zCell)
      allocate(mesh % zCell % ioinfo)
      mesh % zCell % fieldName = 'zCell'
      mesh % zCell % isVarArray = .false.
      mesh % zCell % isPersistent = .true.
      mesh % zCell % isActive = .true.
      allocate(mesh % zCell % array(nCells + 1))
      mesh % zCell % array = 0.0_RKIND
      mesh % zCell % dimSizes(1) = nCells
      mesh % zCell % dimNames(1) = 'nCells'
      mesh % zCell % hasTimeDimension = .false.
      nullify(mesh % zCell % prev)
      nullify(mesh % zCell % next)
      nullify(mesh % zCell % sendList)
      nullify(mesh % zCell % recvList)
      nullify(mesh % zCell % copyList)
      mesh % zCell % ioinfo % input = .true.
      mesh % zCell % ioinfo % sfc = .false.
      mesh % zCell % ioinfo % restart = .true.
      mesh % zCell % ioinfo % output = .true.
      mesh % zCell % block => b

      allocate(mesh % indexToCellID)
      allocate(mesh % indexToCellID % ioinfo)
      mesh % indexToCellID % fieldName = 'indexToCellID'
      mesh % indexToCellID % isVarArray = .false.
      mesh % indexToCellID % isPersistent = .true.
      mesh % indexToCellID % isActive = .true.
      allocate(mesh % indexToCellID % array(nCells + 1))
      mesh % indexToCellID % array = 0
      mesh % indexToCellID % dimSizes(1) = nCells
      mesh % indexToCellID % dimNames(1) = 'nCells'
      mesh % indexToCellID % hasTimeDimension = .false.
      nullify(mesh % indexToCellID % prev)
      nullify(mesh % indexToCellID % next)
      nullify(mesh % indexToCellID % sendList)
      nullify(mesh % indexToCellID % recvList)
      nullify(mesh % indexToCellID % copyList)
      mesh % indexToCellID % ioinfo % input = .true.
      mesh % indexToCellID % ioinfo % sfc = .false.
      mesh % indexToCellID % ioinfo % restart = .true.
      mesh % indexToCellID % ioinfo % output = .true.
      mesh % indexToCellID % block => b

      allocate(mesh % latEdge)
      allocate(mesh % latEdge % ioinfo)
      mesh % latEdge % fieldName = 'latEdge'
      mesh % latEdge % isVarArray = .false.
      mesh % latEdge % isPersistent = .true.
      mesh % latEdge % isActive = .true.
      allocate(mesh % latEdge % array(nEdges + 1))
      mesh % latEdge % array = 0.0_RKIND
      mesh % latEdge % dimSizes(1) = nEdges
      mesh % latEdge % dimNames(1) = 'nEdges'
      mesh % latEdge % hasTimeDimension = .false.
      nullify(mesh % latEdge % prev)
      nullify(mesh % latEdge % next)
      nullify(mesh % latEdge % sendList)
      nullify(mesh % latEdge % recvList)
      nullify(mesh % latEdge % copyList)
      mesh % latEdge % ioinfo % input = .true.
      mesh % latEdge % ioinfo % sfc = .false.
      mesh % latEdge % ioinfo % restart = .true.
      mesh % latEdge % ioinfo % output = .true.
      mesh % latEdge % block => b

      allocate(mesh % lonEdge)
      allocate(mesh % lonEdge % ioinfo)
      mesh % lonEdge % fieldName = 'lonEdge'
      mesh % lonEdge % isVarArray = .false.
      mesh % lonEdge % isPersistent = .true.
      mesh % lonEdge % isActive = .true.
      allocate(mesh % lonEdge % array(nEdges + 1))
      mesh % lonEdge % array = 0.0_RKIND
      mesh % lonEdge % dimSizes(1) = nEdges
      mesh % lonEdge % dimNames(1) = 'nEdges'
      mesh % lonEdge % hasTimeDimension = .false.
      nullify(mesh % lonEdge % prev)
      nullify(mesh % lonEdge % next)
      nullify(mesh % lonEdge % sendList)
      nullify(mesh % lonEdge % recvList)
      nullify(mesh % lonEdge % copyList)
      mesh % lonEdge % ioinfo % input = .true.
      mesh % lonEdge % ioinfo % sfc = .false.
      mesh % lonEdge % ioinfo % restart = .true.
      mesh % lonEdge % ioinfo % output = .true.
      mesh % lonEdge % block => b

      allocate(mesh % xEdge)
      allocate(mesh % xEdge % ioinfo)
      mesh % xEdge % fieldName = 'xEdge'
      mesh % xEdge % isVarArray = .false.
      mesh % xEdge % isPersistent = .true.
      mesh % xEdge % isActive = .true.
      allocate(mesh % xEdge % array(nEdges + 1))
      mesh % xEdge % array = 0.0_RKIND
      mesh % xEdge % dimSizes(1) = nEdges
      mesh % xEdge % dimNames(1) = 'nEdges'
      mesh % xEdge % hasTimeDimension = .false.
      nullify(mesh % xEdge % prev)
      nullify(mesh % xEdge % next)
      nullify(mesh % xEdge % sendList)
      nullify(mesh % xEdge % recvList)
      nullify(mesh % xEdge % copyList)
      mesh % xEdge % ioinfo % input = .true.
      mesh % xEdge % ioinfo % sfc = .false.
      mesh % xEdge % ioinfo % restart = .true.
      mesh % xEdge % ioinfo % output = .true.
      mesh % xEdge % block => b

      allocate(mesh % yEdge)
      allocate(mesh % yEdge % ioinfo)
      mesh % yEdge % fieldName = 'yEdge'
      mesh % yEdge % isVarArray = .false.
      mesh % yEdge % isPersistent = .true.
      mesh % yEdge % isActive = .true.
      allocate(mesh % yEdge % array(nEdges + 1))
      mesh % yEdge % array = 0.0_RKIND
      mesh % yEdge % dimSizes(1) = nEdges
      mesh % yEdge % dimNames(1) = 'nEdges'
      mesh % yEdge % hasTimeDimension = .false.
      nullify(mesh % yEdge % prev)
      nullify(mesh % yEdge % next)
      nullify(mesh % yEdge % sendList)
      nullify(mesh % yEdge % recvList)
      nullify(mesh % yEdge % copyList)
      mesh % yEdge % ioinfo % input = .true.
      mesh % yEdge % ioinfo % sfc = .false.
      mesh % yEdge % ioinfo % restart = .true.
      mesh % yEdge % ioinfo % output = .true.
      mesh % yEdge % block => b

      allocate(mesh % zEdge)
      allocate(mesh % zEdge % ioinfo)
      mesh % zEdge % fieldName = 'zEdge'
      mesh % zEdge % isVarArray = .false.
      mesh % zEdge % isPersistent = .true.
      mesh % zEdge % isActive = .true.
      allocate(mesh % zEdge % array(nEdges + 1))
      mesh % zEdge % array = 0.0_RKIND
      mesh % zEdge % dimSizes(1) = nEdges
      mesh % zEdge % dimNames(1) = 'nEdges'
      mesh % zEdge % hasTimeDimension = .false.
      nullify(mesh % zEdge % prev)
      nullify(mesh % zEdge % next)
      nullify(mesh % zEdge % sendList)
      nullify(mesh % zEdge % recvList)
      nullify(mesh % zEdge % copyList)
      mesh % zEdge % ioinfo % input = .true.
      mesh % zEdge % ioinfo % sfc = .false.
      mesh % zEdge % ioinfo % restart = .true.
      mesh % zEdge % ioinfo % output = .true.
      mesh % zEdge % block => b

      allocate(mesh % indexToEdgeID)
      allocate(mesh % indexToEdgeID % ioinfo)
      mesh % indexToEdgeID % fieldName = 'indexToEdgeID'
      mesh % indexToEdgeID % isVarArray = .false.
      mesh % indexToEdgeID % isPersistent = .true.
      mesh % indexToEdgeID % isActive = .true.
      allocate(mesh % indexToEdgeID % array(nEdges + 1))
      mesh % indexToEdgeID % array = 0
      mesh % indexToEdgeID % dimSizes(1) = nEdges
      mesh % indexToEdgeID % dimNames(1) = 'nEdges'
      mesh % indexToEdgeID % hasTimeDimension = .false.
      nullify(mesh % indexToEdgeID % prev)
      nullify(mesh % indexToEdgeID % next)
      nullify(mesh % indexToEdgeID % sendList)
      nullify(mesh % indexToEdgeID % recvList)
      nullify(mesh % indexToEdgeID % copyList)
      mesh % indexToEdgeID % ioinfo % input = .true.
      mesh % indexToEdgeID % ioinfo % sfc = .false.
      mesh % indexToEdgeID % ioinfo % restart = .true.
      mesh % indexToEdgeID % ioinfo % output = .true.
      mesh % indexToEdgeID % block => b

      allocate(mesh % latVertex)
      allocate(mesh % latVertex % ioinfo)
      mesh % latVertex % fieldName = 'latVertex'
      mesh % latVertex % isVarArray = .false.
      mesh % latVertex % isPersistent = .true.
      mesh % latVertex % isActive = .true.
      allocate(mesh % latVertex % array(nVertices + 1))
      mesh % latVertex % array = 0.0_RKIND
      mesh % latVertex % dimSizes(1) = nVertices
      mesh % latVertex % dimNames(1) = 'nVertices'
      mesh % latVertex % hasTimeDimension = .false.
      nullify(mesh % latVertex % prev)
      nullify(mesh % latVertex % next)
      nullify(mesh % latVertex % sendList)
      nullify(mesh % latVertex % recvList)
      nullify(mesh % latVertex % copyList)
      mesh % latVertex % ioinfo % input = .true.
      mesh % latVertex % ioinfo % sfc = .false.
      mesh % latVertex % ioinfo % restart = .true.
      mesh % latVertex % ioinfo % output = .true.
      mesh % latVertex % block => b

      allocate(mesh % lonVertex)
      allocate(mesh % lonVertex % ioinfo)
      mesh % lonVertex % fieldName = 'lonVertex'
      mesh % lonVertex % isVarArray = .false.
      mesh % lonVertex % isPersistent = .true.
      mesh % lonVertex % isActive = .true.
      allocate(mesh % lonVertex % array(nVertices + 1))
      mesh % lonVertex % array = 0.0_RKIND
      mesh % lonVertex % dimSizes(1) = nVertices
      mesh % lonVertex % dimNames(1) = 'nVertices'
      mesh % lonVertex % hasTimeDimension = .false.
      nullify(mesh % lonVertex % prev)
      nullify(mesh % lonVertex % next)
      nullify(mesh % lonVertex % sendList)
      nullify(mesh % lonVertex % recvList)
      nullify(mesh % lonVertex % copyList)
      mesh % lonVertex % ioinfo % input = .true.
      mesh % lonVertex % ioinfo % sfc = .false.
      mesh % lonVertex % ioinfo % restart = .true.
      mesh % lonVertex % ioinfo % output = .true.
      mesh % lonVertex % block => b

      allocate(mesh % xVertex)
      allocate(mesh % xVertex % ioinfo)
      mesh % xVertex % fieldName = 'xVertex'
      mesh % xVertex % isVarArray = .false.
      mesh % xVertex % isPersistent = .true.
      mesh % xVertex % isActive = .true.
      allocate(mesh % xVertex % array(nVertices + 1))
      mesh % xVertex % array = 0.0_RKIND
      mesh % xVertex % dimSizes(1) = nVertices
      mesh % xVertex % dimNames(1) = 'nVertices'
      mesh % xVertex % hasTimeDimension = .false.
      nullify(mesh % xVertex % prev)
      nullify(mesh % xVertex % next)
      nullify(mesh % xVertex % sendList)
      nullify(mesh % xVertex % recvList)
      nullify(mesh % xVertex % copyList)
      mesh % xVertex % ioinfo % input = .true.
      mesh % xVertex % ioinfo % sfc = .false.
      mesh % xVertex % ioinfo % restart = .true.
      mesh % xVertex % ioinfo % output = .true.
      mesh % xVertex % block => b

      allocate(mesh % yVertex)
      allocate(mesh % yVertex % ioinfo)
      mesh % yVertex % fieldName = 'yVertex'
      mesh % yVertex % isVarArray = .false.
      mesh % yVertex % isPersistent = .true.
      mesh % yVertex % isActive = .true.
      allocate(mesh % yVertex % array(nVertices + 1))
      mesh % yVertex % array = 0.0_RKIND
      mesh % yVertex % dimSizes(1) = nVertices
      mesh % yVertex % dimNames(1) = 'nVertices'
      mesh % yVertex % hasTimeDimension = .false.
      nullify(mesh % yVertex % prev)
      nullify(mesh % yVertex % next)
      nullify(mesh % yVertex % sendList)
      nullify(mesh % yVertex % recvList)
      nullify(mesh % yVertex % copyList)
      mesh % yVertex % ioinfo % input = .true.
      mesh % yVertex % ioinfo % sfc = .false.
      mesh % yVertex % ioinfo % restart = .true.
      mesh % yVertex % ioinfo % output = .true.
      mesh % yVertex % block => b

      allocate(mesh % zVertex)
      allocate(mesh % zVertex % ioinfo)
      mesh % zVertex % fieldName = 'zVertex'
      mesh % zVertex % isVarArray = .false.
      mesh % zVertex % isPersistent = .true.
      mesh % zVertex % isActive = .true.
      allocate(mesh % zVertex % array(nVertices + 1))
      mesh % zVertex % array = 0.0_RKIND
      mesh % zVertex % dimSizes(1) = nVertices
      mesh % zVertex % dimNames(1) = 'nVertices'
      mesh % zVertex % hasTimeDimension = .false.
      nullify(mesh % zVertex % prev)
      nullify(mesh % zVertex % next)
      nullify(mesh % zVertex % sendList)
      nullify(mesh % zVertex % recvList)
      nullify(mesh % zVertex % copyList)
      mesh % zVertex % ioinfo % input = .true.
      mesh % zVertex % ioinfo % sfc = .false.
      mesh % zVertex % ioinfo % restart = .true.
      mesh % zVertex % ioinfo % output = .true.
      mesh % zVertex % block => b

      allocate(mesh % indexToVertexID)
      allocate(mesh % indexToVertexID % ioinfo)
      mesh % indexToVertexID % fieldName = 'indexToVertexID'
      mesh % indexToVertexID % isVarArray = .false.
      mesh % indexToVertexID % isPersistent = .true.
      mesh % indexToVertexID % isActive = .true.
      allocate(mesh % indexToVertexID % array(nVertices + 1))
      mesh % indexToVertexID % array = 0
      mesh % indexToVertexID % dimSizes(1) = nVertices
      mesh % indexToVertexID % dimNames(1) = 'nVertices'
      mesh % indexToVertexID % hasTimeDimension = .false.
      nullify(mesh % indexToVertexID % prev)
      nullify(mesh % indexToVertexID % next)
      nullify(mesh % indexToVertexID % sendList)
      nullify(mesh % indexToVertexID % recvList)
      nullify(mesh % indexToVertexID % copyList)
      mesh % indexToVertexID % ioinfo % input = .true.
      mesh % indexToVertexID % ioinfo % sfc = .false.
      mesh % indexToVertexID % ioinfo % restart = .true.
      mesh % indexToVertexID % ioinfo % output = .true.
      mesh % indexToVertexID % block => b

      allocate(mesh % meshDensity)
      allocate(mesh % meshDensity % ioinfo)
      mesh % meshDensity % fieldName = 'meshDensity'
      mesh % meshDensity % isVarArray = .false.
      mesh % meshDensity % isPersistent = .true.
      mesh % meshDensity % isActive = .true.
      allocate(mesh % meshDensity % array(nCells + 1))
      mesh % meshDensity % array = 0.0_RKIND
      mesh % meshDensity % dimSizes(1) = nCells
      mesh % meshDensity % dimNames(1) = 'nCells'
      mesh % meshDensity % hasTimeDimension = .false.
      nullify(mesh % meshDensity % prev)
      nullify(mesh % meshDensity % next)
      nullify(mesh % meshDensity % sendList)
      nullify(mesh % meshDensity % recvList)
      nullify(mesh % meshDensity % copyList)
      mesh % meshDensity % ioinfo % input = .true.
      mesh % meshDensity % ioinfo % sfc = .false.
      mesh % meshDensity % ioinfo % restart = .true.
      mesh % meshDensity % ioinfo % output = .true.
      mesh % meshDensity % block => b

      allocate(mesh % meshScalingDel2)
      allocate(mesh % meshScalingDel2 % ioinfo)
      mesh % meshScalingDel2 % fieldName = 'meshScalingDel2'
      mesh % meshScalingDel2 % isVarArray = .false.
      mesh % meshScalingDel2 % isPersistent = .true.
      mesh % meshScalingDel2 % isActive = .true.
      allocate(mesh % meshScalingDel2 % array(nEdges + 1))
      mesh % meshScalingDel2 % array = 0.0_RKIND
      mesh % meshScalingDel2 % dimSizes(1) = nEdges
      mesh % meshScalingDel2 % dimNames(1) = 'nEdges'
      mesh % meshScalingDel2 % hasTimeDimension = .false.
      nullify(mesh % meshScalingDel2 % prev)
      nullify(mesh % meshScalingDel2 % next)
      nullify(mesh % meshScalingDel2 % sendList)
      nullify(mesh % meshScalingDel2 % recvList)
      nullify(mesh % meshScalingDel2 % copyList)
      mesh % meshScalingDel2 % ioinfo % input = .false.
      mesh % meshScalingDel2 % ioinfo % sfc = .false.
      mesh % meshScalingDel2 % ioinfo % restart = .true.
      mesh % meshScalingDel2 % ioinfo % output = .true.
      mesh % meshScalingDel2 % block => b

      allocate(mesh % meshScalingDel4)
      allocate(mesh % meshScalingDel4 % ioinfo)
      mesh % meshScalingDel4 % fieldName = 'meshScalingDel4'
      mesh % meshScalingDel4 % isVarArray = .false.
      mesh % meshScalingDel4 % isPersistent = .true.
      mesh % meshScalingDel4 % isActive = .true.
      allocate(mesh % meshScalingDel4 % array(nEdges + 1))
      mesh % meshScalingDel4 % array = 0.0_RKIND
      mesh % meshScalingDel4 % dimSizes(1) = nEdges
      mesh % meshScalingDel4 % dimNames(1) = 'nEdges'
      mesh % meshScalingDel4 % hasTimeDimension = .false.
      nullify(mesh % meshScalingDel4 % prev)
      nullify(mesh % meshScalingDel4 % next)
      nullify(mesh % meshScalingDel4 % sendList)
      nullify(mesh % meshScalingDel4 % recvList)
      nullify(mesh % meshScalingDel4 % copyList)
      mesh % meshScalingDel4 % ioinfo % input = .false.
      mesh % meshScalingDel4 % ioinfo % sfc = .false.
      mesh % meshScalingDel4 % ioinfo % restart = .true.
      mesh % meshScalingDel4 % ioinfo % output = .true.
      mesh % meshScalingDel4 % block => b

      allocate(mesh % meshScaling)
      allocate(mesh % meshScaling % ioinfo)
      mesh % meshScaling % fieldName = 'meshScaling'
      mesh % meshScaling % isVarArray = .false.
      mesh % meshScaling % isPersistent = .true.
      mesh % meshScaling % isActive = .true.
      allocate(mesh % meshScaling % array(nEdges + 1))
      mesh % meshScaling % array = 0.0_RKIND
      mesh % meshScaling % dimSizes(1) = nEdges
      mesh % meshScaling % dimNames(1) = 'nEdges'
      mesh % meshScaling % hasTimeDimension = .false.
      nullify(mesh % meshScaling % prev)
      nullify(mesh % meshScaling % next)
      nullify(mesh % meshScaling % sendList)
      nullify(mesh % meshScaling % recvList)
      nullify(mesh % meshScaling % copyList)
      mesh % meshScaling % ioinfo % input = .false.
      mesh % meshScaling % ioinfo % sfc = .false.
      mesh % meshScaling % ioinfo % restart = .true.
      mesh % meshScaling % ioinfo % output = .true.
      mesh % meshScaling % block => b

      allocate(mesh % cellsOnEdge)
      allocate(mesh % cellsOnEdge % ioinfo)
      mesh % cellsOnEdge % fieldName = 'cellsOnEdge'
      mesh % cellsOnEdge % isVarArray = .false.
      mesh % cellsOnEdge % isPersistent = .true.
      mesh % cellsOnEdge % isActive = .true.
      allocate(mesh % cellsOnEdge % array(2, nEdges + 1))
      mesh % cellsOnEdge % array = 0
      mesh % cellsOnEdge % dimSizes(1) = 2
      mesh % cellsOnEdge % dimNames(1) = 'TWO'
      mesh % cellsOnEdge % dimSizes(2) = nEdges
      mesh % cellsOnEdge % dimNames(2) = 'nEdges'
      mesh % cellsOnEdge % hasTimeDimension = .false.
      nullify(mesh % cellsOnEdge % prev)
      nullify(mesh % cellsOnEdge % next)
      nullify(mesh % cellsOnEdge % sendList)
      nullify(mesh % cellsOnEdge % recvList)
      nullify(mesh % cellsOnEdge % copyList)
      mesh % cellsOnEdge % ioinfo % input = .true.
      mesh % cellsOnEdge % ioinfo % sfc = .false.
      mesh % cellsOnEdge % ioinfo % restart = .true.
      mesh % cellsOnEdge % ioinfo % output = .true.
      mesh % cellsOnEdge % block => b

      allocate(mesh % nEdgesOnCell)
      allocate(mesh % nEdgesOnCell % ioinfo)
      mesh % nEdgesOnCell % fieldName = 'nEdgesOnCell'
      mesh % nEdgesOnCell % isVarArray = .false.
      mesh % nEdgesOnCell % isPersistent = .true.
      mesh % nEdgesOnCell % isActive = .true.
      allocate(mesh % nEdgesOnCell % array(nCells + 1))
      mesh % nEdgesOnCell % array = 0
      mesh % nEdgesOnCell % dimSizes(1) = nCells
      mesh % nEdgesOnCell % dimNames(1) = 'nCells'
      mesh % nEdgesOnCell % hasTimeDimension = .false.
      nullify(mesh % nEdgesOnCell % prev)
      nullify(mesh % nEdgesOnCell % next)
      nullify(mesh % nEdgesOnCell % sendList)
      nullify(mesh % nEdgesOnCell % recvList)
      nullify(mesh % nEdgesOnCell % copyList)
      mesh % nEdgesOnCell % ioinfo % input = .true.
      mesh % nEdgesOnCell % ioinfo % sfc = .false.
      mesh % nEdgesOnCell % ioinfo % restart = .true.
      mesh % nEdgesOnCell % ioinfo % output = .true.
      mesh % nEdgesOnCell % block => b

      allocate(mesh % nEdgesOnEdge)
      allocate(mesh % nEdgesOnEdge % ioinfo)
      mesh % nEdgesOnEdge % fieldName = 'nEdgesOnEdge'
      mesh % nEdgesOnEdge % isVarArray = .false.
      mesh % nEdgesOnEdge % isPersistent = .true.
      mesh % nEdgesOnEdge % isActive = .true.
      allocate(mesh % nEdgesOnEdge % array(nEdges + 1))
      mesh % nEdgesOnEdge % array = 0
      mesh % nEdgesOnEdge % dimSizes(1) = nEdges
      mesh % nEdgesOnEdge % dimNames(1) = 'nEdges'
      mesh % nEdgesOnEdge % hasTimeDimension = .false.
      nullify(mesh % nEdgesOnEdge % prev)
      nullify(mesh % nEdgesOnEdge % next)
      nullify(mesh % nEdgesOnEdge % sendList)
      nullify(mesh % nEdgesOnEdge % recvList)
      nullify(mesh % nEdgesOnEdge % copyList)
      mesh % nEdgesOnEdge % ioinfo % input = .true.
      mesh % nEdgesOnEdge % ioinfo % sfc = .false.
      mesh % nEdgesOnEdge % ioinfo % restart = .true.
      mesh % nEdgesOnEdge % ioinfo % output = .true.
      mesh % nEdgesOnEdge % block => b

      allocate(mesh % edgesOnCell)
      allocate(mesh % edgesOnCell % ioinfo)
      mesh % edgesOnCell % fieldName = 'edgesOnCell'
      mesh % edgesOnCell % isVarArray = .false.
      mesh % edgesOnCell % isPersistent = .true.
      mesh % edgesOnCell % isActive = .true.
      allocate(mesh % edgesOnCell % array(maxEdges, nCells + 1))
      mesh % edgesOnCell % array = 0
      mesh % edgesOnCell % dimSizes(1) = maxEdges
      mesh % edgesOnCell % dimNames(1) = 'maxEdges'
      mesh % edgesOnCell % dimSizes(2) = nCells
      mesh % edgesOnCell % dimNames(2) = 'nCells'
      mesh % edgesOnCell % hasTimeDimension = .false.
      nullify(mesh % edgesOnCell % prev)
      nullify(mesh % edgesOnCell % next)
      nullify(mesh % edgesOnCell % sendList)
      nullify(mesh % edgesOnCell % recvList)
      nullify(mesh % edgesOnCell % copyList)
      mesh % edgesOnCell % ioinfo % input = .true.
      mesh % edgesOnCell % ioinfo % sfc = .false.
      mesh % edgesOnCell % ioinfo % restart = .true.
      mesh % edgesOnCell % ioinfo % output = .true.
      mesh % edgesOnCell % block => b

      allocate(mesh % edgesOnEdge)
      allocate(mesh % edgesOnEdge % ioinfo)
      mesh % edgesOnEdge % fieldName = 'edgesOnEdge'
      mesh % edgesOnEdge % isVarArray = .false.
      mesh % edgesOnEdge % isPersistent = .true.
      mesh % edgesOnEdge % isActive = .true.
      allocate(mesh % edgesOnEdge % array(maxEdges2, nEdges + 1))
      mesh % edgesOnEdge % array = 0
      mesh % edgesOnEdge % dimSizes(1) = maxEdges2
      mesh % edgesOnEdge % dimNames(1) = 'maxEdges2'
      mesh % edgesOnEdge % dimSizes(2) = nEdges
      mesh % edgesOnEdge % dimNames(2) = 'nEdges'
      mesh % edgesOnEdge % hasTimeDimension = .false.
      nullify(mesh % edgesOnEdge % prev)
      nullify(mesh % edgesOnEdge % next)
      nullify(mesh % edgesOnEdge % sendList)
      nullify(mesh % edgesOnEdge % recvList)
      nullify(mesh % edgesOnEdge % copyList)
      mesh % edgesOnEdge % ioinfo % input = .true.
      mesh % edgesOnEdge % ioinfo % sfc = .false.
      mesh % edgesOnEdge % ioinfo % restart = .true.
      mesh % edgesOnEdge % ioinfo % output = .true.
      mesh % edgesOnEdge % block => b

      allocate(mesh % weightsOnEdge)
      allocate(mesh % weightsOnEdge % ioinfo)
      mesh % weightsOnEdge % fieldName = 'weightsOnEdge'
      mesh % weightsOnEdge % isVarArray = .false.
      mesh % weightsOnEdge % isPersistent = .true.
      mesh % weightsOnEdge % isActive = .true.
      allocate(mesh % weightsOnEdge % array(maxEdges2, nEdges + 1))
      mesh % weightsOnEdge % array = 0.0_RKIND
      mesh % weightsOnEdge % dimSizes(1) = maxEdges2
      mesh % weightsOnEdge % dimNames(1) = 'maxEdges2'
      mesh % weightsOnEdge % dimSizes(2) = nEdges
      mesh % weightsOnEdge % dimNames(2) = 'nEdges'
      mesh % weightsOnEdge % hasTimeDimension = .false.
      nullify(mesh % weightsOnEdge % prev)
      nullify(mesh % weightsOnEdge % next)
      nullify(mesh % weightsOnEdge % sendList)
      nullify(mesh % weightsOnEdge % recvList)
      nullify(mesh % weightsOnEdge % copyList)
      mesh % weightsOnEdge % ioinfo % input = .true.
      mesh % weightsOnEdge % ioinfo % sfc = .false.
      mesh % weightsOnEdge % ioinfo % restart = .true.
      mesh % weightsOnEdge % ioinfo % output = .true.
      mesh % weightsOnEdge % block => b

      allocate(mesh % dvEdge)
      allocate(mesh % dvEdge % ioinfo)
      mesh % dvEdge % fieldName = 'dvEdge'
      mesh % dvEdge % isVarArray = .false.
      mesh % dvEdge % isPersistent = .true.
      mesh % dvEdge % isActive = .true.
      allocate(mesh % dvEdge % array(nEdges + 1))
      mesh % dvEdge % array = 0.0_RKIND
      mesh % dvEdge % dimSizes(1) = nEdges
      mesh % dvEdge % dimNames(1) = 'nEdges'
      mesh % dvEdge % hasTimeDimension = .false.
      nullify(mesh % dvEdge % prev)
      nullify(mesh % dvEdge % next)
      nullify(mesh % dvEdge % sendList)
      nullify(mesh % dvEdge % recvList)
      nullify(mesh % dvEdge % copyList)
      mesh % dvEdge % ioinfo % input = .true.
      mesh % dvEdge % ioinfo % sfc = .false.
      mesh % dvEdge % ioinfo % restart = .true.
      mesh % dvEdge % ioinfo % output = .true.
      mesh % dvEdge % block => b

      allocate(mesh % dcEdge)
      allocate(mesh % dcEdge % ioinfo)
      mesh % dcEdge % fieldName = 'dcEdge'
      mesh % dcEdge % isVarArray = .false.
      mesh % dcEdge % isPersistent = .true.
      mesh % dcEdge % isActive = .true.
      allocate(mesh % dcEdge % array(nEdges + 1))
      mesh % dcEdge % array = 0.0_RKIND
      mesh % dcEdge % dimSizes(1) = nEdges
      mesh % dcEdge % dimNames(1) = 'nEdges'
      mesh % dcEdge % hasTimeDimension = .false.
      nullify(mesh % dcEdge % prev)
      nullify(mesh % dcEdge % next)
      nullify(mesh % dcEdge % sendList)
      nullify(mesh % dcEdge % recvList)
      nullify(mesh % dcEdge % copyList)
      mesh % dcEdge % ioinfo % input = .true.
      mesh % dcEdge % ioinfo % sfc = .false.
      mesh % dcEdge % ioinfo % restart = .true.
      mesh % dcEdge % ioinfo % output = .true.
      mesh % dcEdge % block => b

      allocate(mesh % angleEdge)
      allocate(mesh % angleEdge % ioinfo)
      mesh % angleEdge % fieldName = 'angleEdge'
      mesh % angleEdge % isVarArray = .false.
      mesh % angleEdge % isPersistent = .true.
      mesh % angleEdge % isActive = .true.
      allocate(mesh % angleEdge % array(nEdges + 1))
      mesh % angleEdge % array = 0.0_RKIND
      mesh % angleEdge % dimSizes(1) = nEdges
      mesh % angleEdge % dimNames(1) = 'nEdges'
      mesh % angleEdge % hasTimeDimension = .false.
      nullify(mesh % angleEdge % prev)
      nullify(mesh % angleEdge % next)
      nullify(mesh % angleEdge % sendList)
      nullify(mesh % angleEdge % recvList)
      nullify(mesh % angleEdge % copyList)
      mesh % angleEdge % ioinfo % input = .true.
      mesh % angleEdge % ioinfo % sfc = .false.
      mesh % angleEdge % ioinfo % restart = .true.
      mesh % angleEdge % ioinfo % output = .true.
      mesh % angleEdge % block => b

      allocate(mesh % areaCell)
      allocate(mesh % areaCell % ioinfo)
      mesh % areaCell % fieldName = 'areaCell'
      mesh % areaCell % isVarArray = .false.
      mesh % areaCell % isPersistent = .true.
      mesh % areaCell % isActive = .true.
      allocate(mesh % areaCell % array(nCells + 1))
      mesh % areaCell % array = 0.0_RKIND
      mesh % areaCell % dimSizes(1) = nCells
      mesh % areaCell % dimNames(1) = 'nCells'
      mesh % areaCell % hasTimeDimension = .false.
      nullify(mesh % areaCell % prev)
      nullify(mesh % areaCell % next)
      nullify(mesh % areaCell % sendList)
      nullify(mesh % areaCell % recvList)
      nullify(mesh % areaCell % copyList)
      mesh % areaCell % ioinfo % input = .true.
      mesh % areaCell % ioinfo % sfc = .false.
      mesh % areaCell % ioinfo % restart = .true.
      mesh % areaCell % ioinfo % output = .true.
      mesh % areaCell % block => b

      allocate(mesh % areaTriangle)
      allocate(mesh % areaTriangle % ioinfo)
      mesh % areaTriangle % fieldName = 'areaTriangle'
      mesh % areaTriangle % isVarArray = .false.
      mesh % areaTriangle % isPersistent = .true.
      mesh % areaTriangle % isActive = .true.
      allocate(mesh % areaTriangle % array(nVertices + 1))
      mesh % areaTriangle % array = 0.0_RKIND
      mesh % areaTriangle % dimSizes(1) = nVertices
      mesh % areaTriangle % dimNames(1) = 'nVertices'
      mesh % areaTriangle % hasTimeDimension = .false.
      nullify(mesh % areaTriangle % prev)
      nullify(mesh % areaTriangle % next)
      nullify(mesh % areaTriangle % sendList)
      nullify(mesh % areaTriangle % recvList)
      nullify(mesh % areaTriangle % copyList)
      mesh % areaTriangle % ioinfo % input = .true.
      mesh % areaTriangle % ioinfo % sfc = .false.
      mesh % areaTriangle % ioinfo % restart = .true.
      mesh % areaTriangle % ioinfo % output = .true.
      mesh % areaTriangle % block => b

      allocate(mesh % edgeNormalVectors)
      allocate(mesh % edgeNormalVectors % ioinfo)
      mesh % edgeNormalVectors % fieldName = 'edgeNormalVectors'
      mesh % edgeNormalVectors % isVarArray = .false.
      mesh % edgeNormalVectors % isPersistent = .true.
      mesh % edgeNormalVectors % isActive = .true.
      allocate(mesh % edgeNormalVectors % array(3, nEdges + 1))
      mesh % edgeNormalVectors % array = 0.0_RKIND
      mesh % edgeNormalVectors % dimSizes(1) = 3
      mesh % edgeNormalVectors % dimNames(1) = 'R3'
      mesh % edgeNormalVectors % dimSizes(2) = nEdges
      mesh % edgeNormalVectors % dimNames(2) = 'nEdges'
      mesh % edgeNormalVectors % hasTimeDimension = .false.
      nullify(mesh % edgeNormalVectors % prev)
      nullify(mesh % edgeNormalVectors % next)
      nullify(mesh % edgeNormalVectors % sendList)
      nullify(mesh % edgeNormalVectors % recvList)
      nullify(mesh % edgeNormalVectors % copyList)
      mesh % edgeNormalVectors % ioinfo % input = .false.
      mesh % edgeNormalVectors % ioinfo % sfc = .false.
      mesh % edgeNormalVectors % ioinfo % restart = .false.
      mesh % edgeNormalVectors % ioinfo % output = .true.
      mesh % edgeNormalVectors % block => b

      allocate(mesh % edgeTangentVectors)
      allocate(mesh % edgeTangentVectors % ioinfo)
      mesh % edgeTangentVectors % fieldName = 'edgeTangentVectors'
      mesh % edgeTangentVectors % isVarArray = .false.
      mesh % edgeTangentVectors % isPersistent = .true.
      mesh % edgeTangentVectors % isActive = .true.
      allocate(mesh % edgeTangentVectors % array(3, nEdges + 1))
      mesh % edgeTangentVectors % array = 0.0_RKIND
      mesh % edgeTangentVectors % dimSizes(1) = 3
      mesh % edgeTangentVectors % dimNames(1) = 'R3'
      mesh % edgeTangentVectors % dimSizes(2) = nEdges
      mesh % edgeTangentVectors % dimNames(2) = 'nEdges'
      mesh % edgeTangentVectors % hasTimeDimension = .false.
      nullify(mesh % edgeTangentVectors % prev)
      nullify(mesh % edgeTangentVectors % next)
      nullify(mesh % edgeTangentVectors % sendList)
      nullify(mesh % edgeTangentVectors % recvList)
      nullify(mesh % edgeTangentVectors % copyList)
      mesh % edgeTangentVectors % ioinfo % input = .false.
      mesh % edgeTangentVectors % ioinfo % sfc = .false.
      mesh % edgeTangentVectors % ioinfo % restart = .false.
      mesh % edgeTangentVectors % ioinfo % output = .true.
      mesh % edgeTangentVectors % block => b

      allocate(mesh % localVerticalUnitVectors)
      allocate(mesh % localVerticalUnitVectors % ioinfo)
      mesh % localVerticalUnitVectors % fieldName = 'localVerticalUnitVectors'
      mesh % localVerticalUnitVectors % isVarArray = .false.
      mesh % localVerticalUnitVectors % isPersistent = .true.
      mesh % localVerticalUnitVectors % isActive = .true.
      allocate(mesh % localVerticalUnitVectors % array(3, nCells + 1))
      mesh % localVerticalUnitVectors % array = 0.0_RKIND
      mesh % localVerticalUnitVectors % dimSizes(1) = 3
      mesh % localVerticalUnitVectors % dimNames(1) = 'R3'
      mesh % localVerticalUnitVectors % dimSizes(2) = nCells
      mesh % localVerticalUnitVectors % dimNames(2) = 'nCells'
      mesh % localVerticalUnitVectors % hasTimeDimension = .false.
      nullify(mesh % localVerticalUnitVectors % prev)
      nullify(mesh % localVerticalUnitVectors % next)
      nullify(mesh % localVerticalUnitVectors % sendList)
      nullify(mesh % localVerticalUnitVectors % recvList)
      nullify(mesh % localVerticalUnitVectors % copyList)
      mesh % localVerticalUnitVectors % ioinfo % input = .false.
      mesh % localVerticalUnitVectors % ioinfo % sfc = .false.
      mesh % localVerticalUnitVectors % ioinfo % restart = .false.
      mesh % localVerticalUnitVectors % ioinfo % output = .true.
      mesh % localVerticalUnitVectors % block => b

      allocate(mesh % cellTangentPlane)
      allocate(mesh % cellTangentPlane % ioinfo)
      mesh % cellTangentPlane % fieldName = 'cellTangentPlane'
      mesh % cellTangentPlane % isVarArray = .false.
      mesh % cellTangentPlane % isPersistent = .true.
      mesh % cellTangentPlane % isActive = .true.
      allocate(mesh % cellTangentPlane % array(3, 2, nCells + 1))
      mesh % cellTangentPlane % array = 0.0_RKIND
      mesh % cellTangentPlane % dimSizes(1) = 3
      mesh % cellTangentPlane % dimNames(1) = 'R3'
      mesh % cellTangentPlane % dimSizes(2) = 2
      mesh % cellTangentPlane % dimNames(2) = 'TWO'
      mesh % cellTangentPlane % dimSizes(3) = nCells
      mesh % cellTangentPlane % dimNames(3) = 'nCells'
      mesh % cellTangentPlane % hasTimeDimension = .false.
      nullify(mesh % cellTangentPlane % prev)
      nullify(mesh % cellTangentPlane % next)
      nullify(mesh % cellTangentPlane % sendList)
      nullify(mesh % cellTangentPlane % recvList)
      nullify(mesh % cellTangentPlane % copyList)
      mesh % cellTangentPlane % ioinfo % input = .false.
      mesh % cellTangentPlane % ioinfo % sfc = .false.
      mesh % cellTangentPlane % ioinfo % restart = .false.
      mesh % cellTangentPlane % ioinfo % output = .true.
      mesh % cellTangentPlane % block => b

      allocate(mesh % cellsOnCell)
      allocate(mesh % cellsOnCell % ioinfo)
      mesh % cellsOnCell % fieldName = 'cellsOnCell'
      mesh % cellsOnCell % isVarArray = .false.
      mesh % cellsOnCell % isPersistent = .true.
      mesh % cellsOnCell % isActive = .true.
      allocate(mesh % cellsOnCell % array(maxEdges, nCells + 1))
      mesh % cellsOnCell % array = 0
      mesh % cellsOnCell % dimSizes(1) = maxEdges
      mesh % cellsOnCell % dimNames(1) = 'maxEdges'
      mesh % cellsOnCell % dimSizes(2) = nCells
      mesh % cellsOnCell % dimNames(2) = 'nCells'
      mesh % cellsOnCell % hasTimeDimension = .false.
      nullify(mesh % cellsOnCell % prev)
      nullify(mesh % cellsOnCell % next)
      nullify(mesh % cellsOnCell % sendList)
      nullify(mesh % cellsOnCell % recvList)
      nullify(mesh % cellsOnCell % copyList)
      mesh % cellsOnCell % ioinfo % input = .true.
      mesh % cellsOnCell % ioinfo % sfc = .false.
      mesh % cellsOnCell % ioinfo % restart = .true.
      mesh % cellsOnCell % ioinfo % output = .true.
      mesh % cellsOnCell % block => b

      allocate(mesh % verticesOnCell)
      allocate(mesh % verticesOnCell % ioinfo)
      mesh % verticesOnCell % fieldName = 'verticesOnCell'
      mesh % verticesOnCell % isVarArray = .false.
      mesh % verticesOnCell % isPersistent = .true.
      mesh % verticesOnCell % isActive = .true.
      allocate(mesh % verticesOnCell % array(maxEdges, nCells + 1))
      mesh % verticesOnCell % array = 0
      mesh % verticesOnCell % dimSizes(1) = maxEdges
      mesh % verticesOnCell % dimNames(1) = 'maxEdges'
      mesh % verticesOnCell % dimSizes(2) = nCells
      mesh % verticesOnCell % dimNames(2) = 'nCells'
      mesh % verticesOnCell % hasTimeDimension = .false.
      nullify(mesh % verticesOnCell % prev)
      nullify(mesh % verticesOnCell % next)
      nullify(mesh % verticesOnCell % sendList)
      nullify(mesh % verticesOnCell % recvList)
      nullify(mesh % verticesOnCell % copyList)
      mesh % verticesOnCell % ioinfo % input = .true.
      mesh % verticesOnCell % ioinfo % sfc = .false.
      mesh % verticesOnCell % ioinfo % restart = .true.
      mesh % verticesOnCell % ioinfo % output = .true.
      mesh % verticesOnCell % block => b

      allocate(mesh % verticesOnEdge)
      allocate(mesh % verticesOnEdge % ioinfo)
      mesh % verticesOnEdge % fieldName = 'verticesOnEdge'
      mesh % verticesOnEdge % isVarArray = .false.
      mesh % verticesOnEdge % isPersistent = .true.
      mesh % verticesOnEdge % isActive = .true.
      allocate(mesh % verticesOnEdge % array(2, nEdges + 1))
      mesh % verticesOnEdge % array = 0
      mesh % verticesOnEdge % dimSizes(1) = 2
      mesh % verticesOnEdge % dimNames(1) = 'TWO'
      mesh % verticesOnEdge % dimSizes(2) = nEdges
      mesh % verticesOnEdge % dimNames(2) = 'nEdges'
      mesh % verticesOnEdge % hasTimeDimension = .false.
      nullify(mesh % verticesOnEdge % prev)
      nullify(mesh % verticesOnEdge % next)
      nullify(mesh % verticesOnEdge % sendList)
      nullify(mesh % verticesOnEdge % recvList)
      nullify(mesh % verticesOnEdge % copyList)
      mesh % verticesOnEdge % ioinfo % input = .true.
      mesh % verticesOnEdge % ioinfo % sfc = .false.
      mesh % verticesOnEdge % ioinfo % restart = .true.
      mesh % verticesOnEdge % ioinfo % output = .true.
      mesh % verticesOnEdge % block => b

      allocate(mesh % edgesOnVertex)
      allocate(mesh % edgesOnVertex % ioinfo)
      mesh % edgesOnVertex % fieldName = 'edgesOnVertex'
      mesh % edgesOnVertex % isVarArray = .false.
      mesh % edgesOnVertex % isPersistent = .true.
      mesh % edgesOnVertex % isActive = .true.
      allocate(mesh % edgesOnVertex % array(vertexDegree, nVertices + 1))
      mesh % edgesOnVertex % array = 0
      mesh % edgesOnVertex % dimSizes(1) = vertexDegree
      mesh % edgesOnVertex % dimNames(1) = 'vertexDegree'
      mesh % edgesOnVertex % dimSizes(2) = nVertices
      mesh % edgesOnVertex % dimNames(2) = 'nVertices'
      mesh % edgesOnVertex % hasTimeDimension = .false.
      nullify(mesh % edgesOnVertex % prev)
      nullify(mesh % edgesOnVertex % next)
      nullify(mesh % edgesOnVertex % sendList)
      nullify(mesh % edgesOnVertex % recvList)
      nullify(mesh % edgesOnVertex % copyList)
      mesh % edgesOnVertex % ioinfo % input = .true.
      mesh % edgesOnVertex % ioinfo % sfc = .false.
      mesh % edgesOnVertex % ioinfo % restart = .true.
      mesh % edgesOnVertex % ioinfo % output = .true.
      mesh % edgesOnVertex % block => b

      allocate(mesh % cellsOnVertex)
      allocate(mesh % cellsOnVertex % ioinfo)
      mesh % cellsOnVertex % fieldName = 'cellsOnVertex'
      mesh % cellsOnVertex % isVarArray = .false.
      mesh % cellsOnVertex % isPersistent = .true.
      mesh % cellsOnVertex % isActive = .true.
      allocate(mesh % cellsOnVertex % array(vertexDegree, nVertices + 1))
      mesh % cellsOnVertex % array = 0
      mesh % cellsOnVertex % dimSizes(1) = vertexDegree
      mesh % cellsOnVertex % dimNames(1) = 'vertexDegree'
      mesh % cellsOnVertex % dimSizes(2) = nVertices
      mesh % cellsOnVertex % dimNames(2) = 'nVertices'
      mesh % cellsOnVertex % hasTimeDimension = .false.
      nullify(mesh % cellsOnVertex % prev)
      nullify(mesh % cellsOnVertex % next)
      nullify(mesh % cellsOnVertex % sendList)
      nullify(mesh % cellsOnVertex % recvList)
      nullify(mesh % cellsOnVertex % copyList)
      mesh % cellsOnVertex % ioinfo % input = .true.
      mesh % cellsOnVertex % ioinfo % sfc = .false.
      mesh % cellsOnVertex % ioinfo % restart = .true.
      mesh % cellsOnVertex % ioinfo % output = .true.
      mesh % cellsOnVertex % block => b

      allocate(mesh % kiteAreasOnVertex)
      allocate(mesh % kiteAreasOnVertex % ioinfo)
      mesh % kiteAreasOnVertex % fieldName = 'kiteAreasOnVertex'
      mesh % kiteAreasOnVertex % isVarArray = .false.
      mesh % kiteAreasOnVertex % isPersistent = .true.
      mesh % kiteAreasOnVertex % isActive = .true.
      allocate(mesh % kiteAreasOnVertex % array(vertexDegree, nVertices + 1))
      mesh % kiteAreasOnVertex % array = 0.0_RKIND
      mesh % kiteAreasOnVertex % dimSizes(1) = vertexDegree
      mesh % kiteAreasOnVertex % dimNames(1) = 'vertexDegree'
      mesh % kiteAreasOnVertex % dimSizes(2) = nVertices
      mesh % kiteAreasOnVertex % dimNames(2) = 'nVertices'
      mesh % kiteAreasOnVertex % hasTimeDimension = .false.
      nullify(mesh % kiteAreasOnVertex % prev)
      nullify(mesh % kiteAreasOnVertex % next)
      nullify(mesh % kiteAreasOnVertex % sendList)
      nullify(mesh % kiteAreasOnVertex % recvList)
      nullify(mesh % kiteAreasOnVertex % copyList)
      mesh % kiteAreasOnVertex % ioinfo % input = .true.
      mesh % kiteAreasOnVertex % ioinfo % sfc = .false.
      mesh % kiteAreasOnVertex % ioinfo % restart = .true.
      mesh % kiteAreasOnVertex % ioinfo % output = .true.
      mesh % kiteAreasOnVertex % block => b

      allocate(mesh % fEdge)
      allocate(mesh % fEdge % ioinfo)
      mesh % fEdge % fieldName = 'fEdge'
      mesh % fEdge % isVarArray = .false.
      mesh % fEdge % isPersistent = .true.
      mesh % fEdge % isActive = .true.
      allocate(mesh % fEdge % array(nEdges + 1))
      mesh % fEdge % array = 0.0_RKIND
      mesh % fEdge % dimSizes(1) = nEdges
      mesh % fEdge % dimNames(1) = 'nEdges'
      mesh % fEdge % hasTimeDimension = .false.
      nullify(mesh % fEdge % prev)
      nullify(mesh % fEdge % next)
      nullify(mesh % fEdge % sendList)
      nullify(mesh % fEdge % recvList)
      nullify(mesh % fEdge % copyList)
      mesh % fEdge % ioinfo % input = .true.
      mesh % fEdge % ioinfo % sfc = .false.
      mesh % fEdge % ioinfo % restart = .true.
      mesh % fEdge % ioinfo % output = .true.
      mesh % fEdge % block => b

      allocate(mesh % fVertex)
      allocate(mesh % fVertex % ioinfo)
      mesh % fVertex % fieldName = 'fVertex'
      mesh % fVertex % isVarArray = .false.
      mesh % fVertex % isPersistent = .true.
      mesh % fVertex % isActive = .true.
      allocate(mesh % fVertex % array(nVertices + 1))
      mesh % fVertex % array = 0.0_RKIND
      mesh % fVertex % dimSizes(1) = nVertices
      mesh % fVertex % dimNames(1) = 'nVertices'
      mesh % fVertex % hasTimeDimension = .false.
      nullify(mesh % fVertex % prev)
      nullify(mesh % fVertex % next)
      nullify(mesh % fVertex % sendList)
      nullify(mesh % fVertex % recvList)
      nullify(mesh % fVertex % copyList)
      mesh % fVertex % ioinfo % input = .true.
      mesh % fVertex % ioinfo % sfc = .false.
      mesh % fVertex % ioinfo % restart = .true.
      mesh % fVertex % ioinfo % output = .true.
      mesh % fVertex % block => b

      allocate(mesh % fCell)
      allocate(mesh % fCell % ioinfo)
      mesh % fCell % fieldName = 'fCell'
      mesh % fCell % isVarArray = .false.
      mesh % fCell % isPersistent = .true.
      mesh % fCell % isActive = .true.
      allocate(mesh % fCell % array(nCells + 1))
      mesh % fCell % array = 0.0_RKIND
      mesh % fCell % dimSizes(1) = nCells
      mesh % fCell % dimNames(1) = 'nCells'
      mesh % fCell % hasTimeDimension = .false.
      nullify(mesh % fCell % prev)
      nullify(mesh % fCell % next)
      nullify(mesh % fCell % sendList)
      nullify(mesh % fCell % recvList)
      nullify(mesh % fCell % copyList)
      mesh % fCell % ioinfo % input = .true.
      mesh % fCell % ioinfo % sfc = .false.
      mesh % fCell % ioinfo % restart = .true.
      mesh % fCell % ioinfo % output = .true.
      mesh % fCell % block => b

      allocate(mesh % bottomDepth)
      allocate(mesh % bottomDepth % ioinfo)
      mesh % bottomDepth % fieldName = 'bottomDepth'
      mesh % bottomDepth % isVarArray = .false.
      mesh % bottomDepth % isPersistent = .true.
      mesh % bottomDepth % isActive = .true.
      allocate(mesh % bottomDepth % array(nCells + 1))
      mesh % bottomDepth % array = 0.0_RKIND
      mesh % bottomDepth % dimSizes(1) = nCells
      mesh % bottomDepth % dimNames(1) = 'nCells'
      mesh % bottomDepth % hasTimeDimension = .false.
      nullify(mesh % bottomDepth % prev)
      nullify(mesh % bottomDepth % next)
      nullify(mesh % bottomDepth % sendList)
      nullify(mesh % bottomDepth % recvList)
      nullify(mesh % bottomDepth % copyList)
      mesh % bottomDepth % ioinfo % input = .true.
      mesh % bottomDepth % ioinfo % sfc = .false.
      mesh % bottomDepth % ioinfo % restart = .true.
      mesh % bottomDepth % ioinfo % output = .true.
      mesh % bottomDepth % block => b

      allocate(mesh % derivTwo)
      allocate(mesh % derivTwo % ioinfo)
      mesh % derivTwo % fieldName = 'derivTwo'
      mesh % derivTwo % isVarArray = .false.
      mesh % derivTwo % isPersistent = .true.
      mesh % derivTwo % isActive = .true.
      allocate(mesh % derivTwo % array(maxEdges2, 2, nEdges + 1))
      mesh % derivTwo % array = 0.0_RKIND
      mesh % derivTwo % dimSizes(1) = maxEdges2
      mesh % derivTwo % dimNames(1) = 'maxEdges2'
      mesh % derivTwo % dimSizes(2) = 2
      mesh % derivTwo % dimNames(2) = 'TWO'
      mesh % derivTwo % dimSizes(3) = nEdges
      mesh % derivTwo % dimNames(3) = 'nEdges'
      mesh % derivTwo % hasTimeDimension = .false.
      nullify(mesh % derivTwo % prev)
      nullify(mesh % derivTwo % next)
      nullify(mesh % derivTwo % sendList)
      nullify(mesh % derivTwo % recvList)
      nullify(mesh % derivTwo % copyList)
      mesh % derivTwo % ioinfo % input = .false.
      mesh % derivTwo % ioinfo % sfc = .false.
      mesh % derivTwo % ioinfo % restart = .false.
      mesh % derivTwo % ioinfo % output = .false.
      mesh % derivTwo % block => b

      allocate(mesh % advCoefs)
      allocate(mesh % advCoefs % ioinfo)
      mesh % advCoefs % fieldName = 'advCoefs'
      mesh % advCoefs % isVarArray = .false.
      mesh % advCoefs % isPersistent = .true.
      mesh % advCoefs % isActive = .true.
      allocate(mesh % advCoefs % array(maxEdges2+0, nEdges + 1))
      mesh % advCoefs % array = 0.0_RKIND
      mesh % advCoefs % dimSizes(1) = maxEdges2+0
      mesh % advCoefs % dimNames(1) = 'nAdvectionCells'
      mesh % advCoefs % dimSizes(2) = nEdges
      mesh % advCoefs % dimNames(2) = 'nEdges'
      mesh % advCoefs % hasTimeDimension = .false.
      nullify(mesh % advCoefs % prev)
      nullify(mesh % advCoefs % next)
      nullify(mesh % advCoefs % sendList)
      nullify(mesh % advCoefs % recvList)
      nullify(mesh % advCoefs % copyList)
      mesh % advCoefs % ioinfo % input = .false.
      mesh % advCoefs % ioinfo % sfc = .false.
      mesh % advCoefs % ioinfo % restart = .false.
      mesh % advCoefs % ioinfo % output = .false.
      mesh % advCoefs % block => b

      allocate(mesh % advCoefs3rd)
      allocate(mesh % advCoefs3rd % ioinfo)
      mesh % advCoefs3rd % fieldName = 'advCoefs3rd'
      mesh % advCoefs3rd % isVarArray = .false.
      mesh % advCoefs3rd % isPersistent = .true.
      mesh % advCoefs3rd % isActive = .true.
      allocate(mesh % advCoefs3rd % array(maxEdges2+0, nEdges + 1))
      mesh % advCoefs3rd % array = 0.0_RKIND
      mesh % advCoefs3rd % dimSizes(1) = maxEdges2+0
      mesh % advCoefs3rd % dimNames(1) = 'nAdvectionCells'
      mesh % advCoefs3rd % dimSizes(2) = nEdges
      mesh % advCoefs3rd % dimNames(2) = 'nEdges'
      mesh % advCoefs3rd % hasTimeDimension = .false.
      nullify(mesh % advCoefs3rd % prev)
      nullify(mesh % advCoefs3rd % next)
      nullify(mesh % advCoefs3rd % sendList)
      nullify(mesh % advCoefs3rd % recvList)
      nullify(mesh % advCoefs3rd % copyList)
      mesh % advCoefs3rd % ioinfo % input = .false.
      mesh % advCoefs3rd % ioinfo % sfc = .false.
      mesh % advCoefs3rd % ioinfo % restart = .false.
      mesh % advCoefs3rd % ioinfo % output = .false.
      mesh % advCoefs3rd % block => b

      allocate(mesh % advCellsForEdge)
      allocate(mesh % advCellsForEdge % ioinfo)
      mesh % advCellsForEdge % fieldName = 'advCellsForEdge'
      mesh % advCellsForEdge % isVarArray = .false.
      mesh % advCellsForEdge % isPersistent = .true.
      mesh % advCellsForEdge % isActive = .true.
      allocate(mesh % advCellsForEdge % array(maxEdges2+0, nEdges + 1))
      mesh % advCellsForEdge % array = 0
      mesh % advCellsForEdge % dimSizes(1) = maxEdges2+0
      mesh % advCellsForEdge % dimNames(1) = 'nAdvectionCells'
      mesh % advCellsForEdge % dimSizes(2) = nEdges
      mesh % advCellsForEdge % dimNames(2) = 'nEdges'
      mesh % advCellsForEdge % hasTimeDimension = .false.
      nullify(mesh % advCellsForEdge % prev)
      nullify(mesh % advCellsForEdge % next)
      nullify(mesh % advCellsForEdge % sendList)
      nullify(mesh % advCellsForEdge % recvList)
      nullify(mesh % advCellsForEdge % copyList)
      mesh % advCellsForEdge % ioinfo % input = .false.
      mesh % advCellsForEdge % ioinfo % sfc = .false.
      mesh % advCellsForEdge % ioinfo % restart = .false.
      mesh % advCellsForEdge % ioinfo % output = .false.
      mesh % advCellsForEdge % block => b

      allocate(mesh % nAdvCellsForEdge)
      allocate(mesh % nAdvCellsForEdge % ioinfo)
      mesh % nAdvCellsForEdge % fieldName = 'nAdvCellsForEdge'
      mesh % nAdvCellsForEdge % isVarArray = .false.
      mesh % nAdvCellsForEdge % isPersistent = .true.
      mesh % nAdvCellsForEdge % isActive = .true.
      allocate(mesh % nAdvCellsForEdge % array(nEdges + 1))
      mesh % nAdvCellsForEdge % array = 0
      mesh % nAdvCellsForEdge % dimSizes(1) = nEdges
      mesh % nAdvCellsForEdge % dimNames(1) = 'nEdges'
      mesh % nAdvCellsForEdge % hasTimeDimension = .false.
      nullify(mesh % nAdvCellsForEdge % prev)
      nullify(mesh % nAdvCellsForEdge % next)
      nullify(mesh % nAdvCellsForEdge % sendList)
      nullify(mesh % nAdvCellsForEdge % recvList)
      nullify(mesh % nAdvCellsForEdge % copyList)
      mesh % nAdvCellsForEdge % ioinfo % input = .false.
      mesh % nAdvCellsForEdge % ioinfo % sfc = .false.
      mesh % nAdvCellsForEdge % ioinfo % restart = .false.
      mesh % nAdvCellsForEdge % ioinfo % output = .false.
      mesh % nAdvCellsForEdge % block => b

      allocate(mesh % highOrderAdvectionMask)
      allocate(mesh % highOrderAdvectionMask % ioinfo)
      mesh % highOrderAdvectionMask % fieldName = 'highOrderAdvectionMask'
      mesh % highOrderAdvectionMask % isVarArray = .false.
      mesh % highOrderAdvectionMask % isPersistent = .true.
      mesh % highOrderAdvectionMask % isActive = .true.
      allocate(mesh % highOrderAdvectionMask % array(nVertLevels, nEdges + 1))
      mesh % highOrderAdvectionMask % array = 0
      mesh % highOrderAdvectionMask % dimSizes(1) = nVertLevels
      mesh % highOrderAdvectionMask % dimNames(1) = 'nVertLevels'
      mesh % highOrderAdvectionMask % dimSizes(2) = nEdges
      mesh % highOrderAdvectionMask % dimNames(2) = 'nEdges'
      mesh % highOrderAdvectionMask % hasTimeDimension = .false.
      nullify(mesh % highOrderAdvectionMask % prev)
      nullify(mesh % highOrderAdvectionMask % next)
      nullify(mesh % highOrderAdvectionMask % sendList)
      nullify(mesh % highOrderAdvectionMask % recvList)
      nullify(mesh % highOrderAdvectionMask % copyList)
      mesh % highOrderAdvectionMask % ioinfo % input = .false.
      mesh % highOrderAdvectionMask % ioinfo % sfc = .false.
      mesh % highOrderAdvectionMask % ioinfo % restart = .false.
      mesh % highOrderAdvectionMask % ioinfo % output = .false.
      mesh % highOrderAdvectionMask % block => b

      allocate(mesh % coeffs_reconstruct)
      allocate(mesh % coeffs_reconstruct % ioinfo)
      mesh % coeffs_reconstruct % fieldName = 'coeffs_reconstruct'
      mesh % coeffs_reconstruct % isVarArray = .false.
      mesh % coeffs_reconstruct % isPersistent = .true.
      mesh % coeffs_reconstruct % isActive = .true.
      allocate(mesh % coeffs_reconstruct % array(3, maxEdges, nCells + 1))
      mesh % coeffs_reconstruct % array = 0.0_RKIND
      mesh % coeffs_reconstruct % dimSizes(1) = 3
      mesh % coeffs_reconstruct % dimNames(1) = 'R3'
      mesh % coeffs_reconstruct % dimSizes(2) = maxEdges
      mesh % coeffs_reconstruct % dimNames(2) = 'maxEdges'
      mesh % coeffs_reconstruct % dimSizes(3) = nCells
      mesh % coeffs_reconstruct % dimNames(3) = 'nCells'
      mesh % coeffs_reconstruct % hasTimeDimension = .false.
      nullify(mesh % coeffs_reconstruct % prev)
      nullify(mesh % coeffs_reconstruct % next)
      nullify(mesh % coeffs_reconstruct % sendList)
      nullify(mesh % coeffs_reconstruct % recvList)
      nullify(mesh % coeffs_reconstruct % copyList)
      mesh % coeffs_reconstruct % ioinfo % input = .false.
      mesh % coeffs_reconstruct % ioinfo % sfc = .false.
      mesh % coeffs_reconstruct % ioinfo % restart = .false.
      mesh % coeffs_reconstruct % ioinfo % output = .false.
      mesh % coeffs_reconstruct % block => b

      allocate(mesh % maxLevelCell)
      allocate(mesh % maxLevelCell % ioinfo)
      mesh % maxLevelCell % fieldName = 'maxLevelCell'
      mesh % maxLevelCell % isVarArray = .false.
      mesh % maxLevelCell % isPersistent = .true.
      mesh % maxLevelCell % isActive = .true.
      allocate(mesh % maxLevelCell % array(nCells + 1))
      mesh % maxLevelCell % array = 0
      mesh % maxLevelCell % dimSizes(1) = nCells
      mesh % maxLevelCell % dimNames(1) = 'nCells'
      mesh % maxLevelCell % hasTimeDimension = .false.
      nullify(mesh % maxLevelCell % prev)
      nullify(mesh % maxLevelCell % next)
      nullify(mesh % maxLevelCell % sendList)
      nullify(mesh % maxLevelCell % recvList)
      nullify(mesh % maxLevelCell % copyList)
      mesh % maxLevelCell % ioinfo % input = .true.
      mesh % maxLevelCell % ioinfo % sfc = .false.
      mesh % maxLevelCell % ioinfo % restart = .true.
      mesh % maxLevelCell % ioinfo % output = .true.
      mesh % maxLevelCell % block => b

      allocate(mesh % maxLevelEdgeTop)
      allocate(mesh % maxLevelEdgeTop % ioinfo)
      mesh % maxLevelEdgeTop % fieldName = 'maxLevelEdgeTop'
      mesh % maxLevelEdgeTop % isVarArray = .false.
      mesh % maxLevelEdgeTop % isPersistent = .true.
      mesh % maxLevelEdgeTop % isActive = .true.
      allocate(mesh % maxLevelEdgeTop % array(nEdges + 1))
      mesh % maxLevelEdgeTop % array = 0
      mesh % maxLevelEdgeTop % dimSizes(1) = nEdges
      mesh % maxLevelEdgeTop % dimNames(1) = 'nEdges'
      mesh % maxLevelEdgeTop % hasTimeDimension = .false.
      nullify(mesh % maxLevelEdgeTop % prev)
      nullify(mesh % maxLevelEdgeTop % next)
      nullify(mesh % maxLevelEdgeTop % sendList)
      nullify(mesh % maxLevelEdgeTop % recvList)
      nullify(mesh % maxLevelEdgeTop % copyList)
      mesh % maxLevelEdgeTop % ioinfo % input = .false.
      mesh % maxLevelEdgeTop % ioinfo % sfc = .false.
      mesh % maxLevelEdgeTop % ioinfo % restart = .false.
      mesh % maxLevelEdgeTop % ioinfo % output = .false.
      mesh % maxLevelEdgeTop % block => b

      allocate(mesh % maxLevelEdgeBot)
      allocate(mesh % maxLevelEdgeBot % ioinfo)
      mesh % maxLevelEdgeBot % fieldName = 'maxLevelEdgeBot'
      mesh % maxLevelEdgeBot % isVarArray = .false.
      mesh % maxLevelEdgeBot % isPersistent = .true.
      mesh % maxLevelEdgeBot % isActive = .true.
      allocate(mesh % maxLevelEdgeBot % array(nEdges + 1))
      mesh % maxLevelEdgeBot % array = 0
      mesh % maxLevelEdgeBot % dimSizes(1) = nEdges
      mesh % maxLevelEdgeBot % dimNames(1) = 'nEdges'
      mesh % maxLevelEdgeBot % hasTimeDimension = .false.
      nullify(mesh % maxLevelEdgeBot % prev)
      nullify(mesh % maxLevelEdgeBot % next)
      nullify(mesh % maxLevelEdgeBot % sendList)
      nullify(mesh % maxLevelEdgeBot % recvList)
      nullify(mesh % maxLevelEdgeBot % copyList)
      mesh % maxLevelEdgeBot % ioinfo % input = .false.
      mesh % maxLevelEdgeBot % ioinfo % sfc = .false.
      mesh % maxLevelEdgeBot % ioinfo % restart = .false.
      mesh % maxLevelEdgeBot % ioinfo % output = .false.
      mesh % maxLevelEdgeBot % block => b

      allocate(mesh % maxLevelVertexTop)
      allocate(mesh % maxLevelVertexTop % ioinfo)
      mesh % maxLevelVertexTop % fieldName = 'maxLevelVertexTop'
      mesh % maxLevelVertexTop % isVarArray = .false.
      mesh % maxLevelVertexTop % isPersistent = .true.
      mesh % maxLevelVertexTop % isActive = .true.
      allocate(mesh % maxLevelVertexTop % array(nVertices + 1))
      mesh % maxLevelVertexTop % array = 0
      mesh % maxLevelVertexTop % dimSizes(1) = nVertices
      mesh % maxLevelVertexTop % dimNames(1) = 'nVertices'
      mesh % maxLevelVertexTop % hasTimeDimension = .false.
      nullify(mesh % maxLevelVertexTop % prev)
      nullify(mesh % maxLevelVertexTop % next)
      nullify(mesh % maxLevelVertexTop % sendList)
      nullify(mesh % maxLevelVertexTop % recvList)
      nullify(mesh % maxLevelVertexTop % copyList)
      mesh % maxLevelVertexTop % ioinfo % input = .false.
      mesh % maxLevelVertexTop % ioinfo % sfc = .false.
      mesh % maxLevelVertexTop % ioinfo % restart = .false.
      mesh % maxLevelVertexTop % ioinfo % output = .false.
      mesh % maxLevelVertexTop % block => b

      allocate(mesh % maxLevelVertexBot)
      allocate(mesh % maxLevelVertexBot % ioinfo)
      mesh % maxLevelVertexBot % fieldName = 'maxLevelVertexBot'
      mesh % maxLevelVertexBot % isVarArray = .false.
      mesh % maxLevelVertexBot % isPersistent = .true.
      mesh % maxLevelVertexBot % isActive = .true.
      allocate(mesh % maxLevelVertexBot % array(nVertices + 1))
      mesh % maxLevelVertexBot % array = 0
      mesh % maxLevelVertexBot % dimSizes(1) = nVertices
      mesh % maxLevelVertexBot % dimNames(1) = 'nVertices'
      mesh % maxLevelVertexBot % hasTimeDimension = .false.
      nullify(mesh % maxLevelVertexBot % prev)
      nullify(mesh % maxLevelVertexBot % next)
      nullify(mesh % maxLevelVertexBot % sendList)
      nullify(mesh % maxLevelVertexBot % recvList)
      nullify(mesh % maxLevelVertexBot % copyList)
      mesh % maxLevelVertexBot % ioinfo % input = .false.
      mesh % maxLevelVertexBot % ioinfo % sfc = .false.
      mesh % maxLevelVertexBot % ioinfo % restart = .false.
      mesh % maxLevelVertexBot % ioinfo % output = .false.
      mesh % maxLevelVertexBot % block => b

      allocate(mesh % refBottomDepth)
      allocate(mesh % refBottomDepth % ioinfo)
      mesh % refBottomDepth % fieldName = 'refBottomDepth'
      mesh % refBottomDepth % isVarArray = .false.
      mesh % refBottomDepth % isPersistent = .true.
      mesh % refBottomDepth % isActive = .true.
      allocate(mesh % refBottomDepth % array(nVertLevels))
      mesh % refBottomDepth % array = 0.0_RKIND
      mesh % refBottomDepth % dimSizes(1) = nVertLevels
      mesh % refBottomDepth % dimNames(1) = 'nVertLevels'
      mesh % refBottomDepth % hasTimeDimension = .false.
      nullify(mesh % refBottomDepth % prev)
      nullify(mesh % refBottomDepth % next)
      nullify(mesh % refBottomDepth % sendList)
      nullify(mesh % refBottomDepth % recvList)
      nullify(mesh % refBottomDepth % copyList)
      mesh % refBottomDepth % ioinfo % input = .true.
      mesh % refBottomDepth % ioinfo % sfc = .false.
      mesh % refBottomDepth % ioinfo % restart = .true.
      mesh % refBottomDepth % ioinfo % output = .true.
      mesh % refBottomDepth % block => b

      allocate(mesh % refBottomDepthTopOfCell)
      allocate(mesh % refBottomDepthTopOfCell % ioinfo)
      mesh % refBottomDepthTopOfCell % fieldName = 'refBottomDepthTopOfCell'
      mesh % refBottomDepthTopOfCell % isVarArray = .false.
      mesh % refBottomDepthTopOfCell % isPersistent = .true.
      mesh % refBottomDepthTopOfCell % isActive = .true.
      allocate(mesh % refBottomDepthTopOfCell % array(nVertLevels+1))
      mesh % refBottomDepthTopOfCell % array = 0.0_RKIND
      mesh % refBottomDepthTopOfCell % dimSizes(1) = nVertLevels+1
      mesh % refBottomDepthTopOfCell % dimNames(1) = 'nVertLevelsP1'
      mesh % refBottomDepthTopOfCell % hasTimeDimension = .false.
      nullify(mesh % refBottomDepthTopOfCell % prev)
      nullify(mesh % refBottomDepthTopOfCell % next)
      nullify(mesh % refBottomDepthTopOfCell % sendList)
      nullify(mesh % refBottomDepthTopOfCell % recvList)
      nullify(mesh % refBottomDepthTopOfCell % copyList)
      mesh % refBottomDepthTopOfCell % ioinfo % input = .false.
      mesh % refBottomDepthTopOfCell % ioinfo % sfc = .false.
      mesh % refBottomDepthTopOfCell % ioinfo % restart = .false.
      mesh % refBottomDepthTopOfCell % ioinfo % output = .false.
      mesh % refBottomDepthTopOfCell % block => b

      allocate(mesh % vertCoordMovementWeights)
      allocate(mesh % vertCoordMovementWeights % ioinfo)
      mesh % vertCoordMovementWeights % fieldName = 'vertCoordMovementWeights'
      mesh % vertCoordMovementWeights % isVarArray = .false.
      mesh % vertCoordMovementWeights % isPersistent = .true.
      mesh % vertCoordMovementWeights % isActive = .true.
      allocate(mesh % vertCoordMovementWeights % array(nVertLevels))
      mesh % vertCoordMovementWeights % array = 0.0_RKIND
      mesh % vertCoordMovementWeights % dimSizes(1) = nVertLevels
      mesh % vertCoordMovementWeights % dimNames(1) = 'nVertLevels'
      mesh % vertCoordMovementWeights % hasTimeDimension = .false.
      nullify(mesh % vertCoordMovementWeights % prev)
      nullify(mesh % vertCoordMovementWeights % next)
      nullify(mesh % vertCoordMovementWeights % sendList)
      nullify(mesh % vertCoordMovementWeights % recvList)
      nullify(mesh % vertCoordMovementWeights % copyList)
      mesh % vertCoordMovementWeights % ioinfo % input = .false.
      mesh % vertCoordMovementWeights % ioinfo % sfc = .false.
      mesh % vertCoordMovementWeights % ioinfo % restart = .true.
      mesh % vertCoordMovementWeights % ioinfo % output = .true.
      mesh % vertCoordMovementWeights % block => b

      allocate(mesh % boundaryEdge)
      allocate(mesh % boundaryEdge % ioinfo)
      mesh % boundaryEdge % fieldName = 'boundaryEdge'
      mesh % boundaryEdge % isVarArray = .false.
      mesh % boundaryEdge % isPersistent = .true.
      mesh % boundaryEdge % isActive = .true.
      allocate(mesh % boundaryEdge % array(nVertLevels, nEdges + 1))
      mesh % boundaryEdge % array = 0
      mesh % boundaryEdge % dimSizes(1) = nVertLevels
      mesh % boundaryEdge % dimNames(1) = 'nVertLevels'
      mesh % boundaryEdge % dimSizes(2) = nEdges
      mesh % boundaryEdge % dimNames(2) = 'nEdges'
      mesh % boundaryEdge % hasTimeDimension = .false.
      nullify(mesh % boundaryEdge % prev)
      nullify(mesh % boundaryEdge % next)
      nullify(mesh % boundaryEdge % sendList)
      nullify(mesh % boundaryEdge % recvList)
      nullify(mesh % boundaryEdge % copyList)
      mesh % boundaryEdge % ioinfo % input = .false.
      mesh % boundaryEdge % ioinfo % sfc = .false.
      mesh % boundaryEdge % ioinfo % restart = .false.
      mesh % boundaryEdge % ioinfo % output = .false.
      mesh % boundaryEdge % block => b

      allocate(mesh % boundaryVertex)
      allocate(mesh % boundaryVertex % ioinfo)
      mesh % boundaryVertex % fieldName = 'boundaryVertex'
      mesh % boundaryVertex % isVarArray = .false.
      mesh % boundaryVertex % isPersistent = .true.
      mesh % boundaryVertex % isActive = .true.
      allocate(mesh % boundaryVertex % array(nVertLevels, nVertices + 1))
      mesh % boundaryVertex % array = 0
      mesh % boundaryVertex % dimSizes(1) = nVertLevels
      mesh % boundaryVertex % dimNames(1) = 'nVertLevels'
      mesh % boundaryVertex % dimSizes(2) = nVertices
      mesh % boundaryVertex % dimNames(2) = 'nVertices'
      mesh % boundaryVertex % hasTimeDimension = .false.
      nullify(mesh % boundaryVertex % prev)
      nullify(mesh % boundaryVertex % next)
      nullify(mesh % boundaryVertex % sendList)
      nullify(mesh % boundaryVertex % recvList)
      nullify(mesh % boundaryVertex % copyList)
      mesh % boundaryVertex % ioinfo % input = .false.
      mesh % boundaryVertex % ioinfo % sfc = .false.
      mesh % boundaryVertex % ioinfo % restart = .false.
      mesh % boundaryVertex % ioinfo % output = .false.
      mesh % boundaryVertex % block => b

      allocate(mesh % boundaryCell)
      allocate(mesh % boundaryCell % ioinfo)
      mesh % boundaryCell % fieldName = 'boundaryCell'
      mesh % boundaryCell % isVarArray = .false.
      mesh % boundaryCell % isPersistent = .true.
      mesh % boundaryCell % isActive = .true.
      allocate(mesh % boundaryCell % array(nVertLevels, nCells + 1))
      mesh % boundaryCell % array = 0
      mesh % boundaryCell % dimSizes(1) = nVertLevels
      mesh % boundaryCell % dimNames(1) = 'nVertLevels'
      mesh % boundaryCell % dimSizes(2) = nCells
      mesh % boundaryCell % dimNames(2) = 'nCells'
      mesh % boundaryCell % hasTimeDimension = .false.
      nullify(mesh % boundaryCell % prev)
      nullify(mesh % boundaryCell % next)
      nullify(mesh % boundaryCell % sendList)
      nullify(mesh % boundaryCell % recvList)
      nullify(mesh % boundaryCell % copyList)
      mesh % boundaryCell % ioinfo % input = .false.
      mesh % boundaryCell % ioinfo % sfc = .false.
      mesh % boundaryCell % ioinfo % restart = .false.
      mesh % boundaryCell % ioinfo % output = .false.
      mesh % boundaryCell % block => b

      allocate(mesh % edgeMask)
      allocate(mesh % edgeMask % ioinfo)
      mesh % edgeMask % fieldName = 'edgeMask'
      mesh % edgeMask % isVarArray = .false.
      mesh % edgeMask % isPersistent = .true.
      mesh % edgeMask % isActive = .true.
      allocate(mesh % edgeMask % array(nVertLevels, nEdges + 1))
      mesh % edgeMask % array = 0
      mesh % edgeMask % dimSizes(1) = nVertLevels
      mesh % edgeMask % dimNames(1) = 'nVertLevels'
      mesh % edgeMask % dimSizes(2) = nEdges
      mesh % edgeMask % dimNames(2) = 'nEdges'
      mesh % edgeMask % hasTimeDimension = .false.
      nullify(mesh % edgeMask % prev)
      nullify(mesh % edgeMask % next)
      nullify(mesh % edgeMask % sendList)
      nullify(mesh % edgeMask % recvList)
      nullify(mesh % edgeMask % copyList)
      mesh % edgeMask % ioinfo % input = .false.
      mesh % edgeMask % ioinfo % sfc = .false.
      mesh % edgeMask % ioinfo % restart = .false.
      mesh % edgeMask % ioinfo % output = .true.
      mesh % edgeMask % block => b

      allocate(mesh % vertexMask)
      allocate(mesh % vertexMask % ioinfo)
      mesh % vertexMask % fieldName = 'vertexMask'
      mesh % vertexMask % isVarArray = .false.
      mesh % vertexMask % isPersistent = .true.
      mesh % vertexMask % isActive = .true.
      allocate(mesh % vertexMask % array(nVertLevels, nVertices + 1))
      mesh % vertexMask % array = 0
      mesh % vertexMask % dimSizes(1) = nVertLevels
      mesh % vertexMask % dimNames(1) = 'nVertLevels'
      mesh % vertexMask % dimSizes(2) = nVertices
      mesh % vertexMask % dimNames(2) = 'nVertices'
      mesh % vertexMask % hasTimeDimension = .false.
      nullify(mesh % vertexMask % prev)
      nullify(mesh % vertexMask % next)
      nullify(mesh % vertexMask % sendList)
      nullify(mesh % vertexMask % recvList)
      nullify(mesh % vertexMask % copyList)
      mesh % vertexMask % ioinfo % input = .false.
      mesh % vertexMask % ioinfo % sfc = .false.
      mesh % vertexMask % ioinfo % restart = .false.
      mesh % vertexMask % ioinfo % output = .true.
      mesh % vertexMask % block => b

      allocate(mesh % cellMask)
      allocate(mesh % cellMask % ioinfo)
      mesh % cellMask % fieldName = 'cellMask'
      mesh % cellMask % isVarArray = .false.
      mesh % cellMask % isPersistent = .true.
      mesh % cellMask % isActive = .true.
      allocate(mesh % cellMask % array(nVertLevels, nCells + 1))
      mesh % cellMask % array = 0
      mesh % cellMask % dimSizes(1) = nVertLevels
      mesh % cellMask % dimNames(1) = 'nVertLevels'
      mesh % cellMask % dimSizes(2) = nCells
      mesh % cellMask % dimNames(2) = 'nCells'
      mesh % cellMask % hasTimeDimension = .false.
      nullify(mesh % cellMask % prev)
      nullify(mesh % cellMask % next)
      nullify(mesh % cellMask % sendList)
      nullify(mesh % cellMask % recvList)
      nullify(mesh % cellMask % copyList)
      mesh % cellMask % ioinfo % input = .false.
      mesh % cellMask % ioinfo % sfc = .false.
      mesh % cellMask % ioinfo % restart = .false.
      mesh % cellMask % ioinfo % output = .true.
      mesh % cellMask % block => b

      allocate(mesh % temperatureRestore)
      allocate(mesh % temperatureRestore % ioinfo)
      mesh % temperatureRestore % fieldName = 'temperatureRestore'
      mesh % temperatureRestore % isVarArray = .false.
      mesh % temperatureRestore % isPersistent = .true.
      mesh % temperatureRestore % isActive = .true.
      allocate(mesh % temperatureRestore % array(nCells + 1))
      mesh % temperatureRestore % array = 0.0_RKIND
      mesh % temperatureRestore % dimSizes(1) = nCells
      mesh % temperatureRestore % dimNames(1) = 'nCells'
      mesh % temperatureRestore % hasTimeDimension = .false.
      nullify(mesh % temperatureRestore % prev)
      nullify(mesh % temperatureRestore % next)
      nullify(mesh % temperatureRestore % sendList)
      nullify(mesh % temperatureRestore % recvList)
      nullify(mesh % temperatureRestore % copyList)
      mesh % temperatureRestore % ioinfo % input = .true.
      mesh % temperatureRestore % ioinfo % sfc = .false.
      mesh % temperatureRestore % ioinfo % restart = .true.
      mesh % temperatureRestore % ioinfo % output = .false.
      mesh % temperatureRestore % block => b

      allocate(mesh % salinityRestore)
      allocate(mesh % salinityRestore % ioinfo)
      mesh % salinityRestore % fieldName = 'salinityRestore'
      mesh % salinityRestore % isVarArray = .false.
      mesh % salinityRestore % isPersistent = .true.
      mesh % salinityRestore % isActive = .true.
      allocate(mesh % salinityRestore % array(nCells + 1))
      mesh % salinityRestore % array = 0.0_RKIND
      mesh % salinityRestore % dimSizes(1) = nCells
      mesh % salinityRestore % dimNames(1) = 'nCells'
      mesh % salinityRestore % hasTimeDimension = .false.
      nullify(mesh % salinityRestore % prev)
      nullify(mesh % salinityRestore % next)
      nullify(mesh % salinityRestore % sendList)
      nullify(mesh % salinityRestore % recvList)
      nullify(mesh % salinityRestore % copyList)
      mesh % salinityRestore % ioinfo % input = .true.
      mesh % salinityRestore % ioinfo % sfc = .false.
      mesh % salinityRestore % ioinfo % restart = .true.
      mesh % salinityRestore % ioinfo % output = .false.
      mesh % salinityRestore % block => b

      allocate(mesh % edgeSignOnCell)
      allocate(mesh % edgeSignOnCell % ioinfo)
      mesh % edgeSignOnCell % fieldName = 'edgeSignOnCell'
      mesh % edgeSignOnCell % isVarArray = .false.
      mesh % edgeSignOnCell % isPersistent = .true.
      mesh % edgeSignOnCell % isActive = .true.
      allocate(mesh % edgeSignOnCell % array(maxEdges, nCells + 1))
      mesh % edgeSignOnCell % array = 0
      mesh % edgeSignOnCell % dimSizes(1) = maxEdges
      mesh % edgeSignOnCell % dimNames(1) = 'maxEdges'
      mesh % edgeSignOnCell % dimSizes(2) = nCells
      mesh % edgeSignOnCell % dimNames(2) = 'nCells'
      mesh % edgeSignOnCell % hasTimeDimension = .false.
      nullify(mesh % edgeSignOnCell % prev)
      nullify(mesh % edgeSignOnCell % next)
      nullify(mesh % edgeSignOnCell % sendList)
      nullify(mesh % edgeSignOnCell % recvList)
      nullify(mesh % edgeSignOnCell % copyList)
      mesh % edgeSignOnCell % ioinfo % input = .false.
      mesh % edgeSignOnCell % ioinfo % sfc = .false.
      mesh % edgeSignOnCell % ioinfo % restart = .false.
      mesh % edgeSignOnCell % ioinfo % output = .false.
      mesh % edgeSignOnCell % block => b

      allocate(mesh % edgeSignOnVertex)
      allocate(mesh % edgeSignOnVertex % ioinfo)
      mesh % edgeSignOnVertex % fieldName = 'edgeSignOnVertex'
      mesh % edgeSignOnVertex % isVarArray = .false.
      mesh % edgeSignOnVertex % isPersistent = .true.
      mesh % edgeSignOnVertex % isActive = .true.
      allocate(mesh % edgeSignOnVertex % array(maxEdges, nVertices + 1))
      mesh % edgeSignOnVertex % array = 0
      mesh % edgeSignOnVertex % dimSizes(1) = maxEdges
      mesh % edgeSignOnVertex % dimNames(1) = 'maxEdges'
      mesh % edgeSignOnVertex % dimSizes(2) = nVertices
      mesh % edgeSignOnVertex % dimNames(2) = 'nVertices'
      mesh % edgeSignOnVertex % hasTimeDimension = .false.
      nullify(mesh % edgeSignOnVertex % prev)
      nullify(mesh % edgeSignOnVertex % next)
      nullify(mesh % edgeSignOnVertex % sendList)
      nullify(mesh % edgeSignOnVertex % recvList)
      nullify(mesh % edgeSignOnVertex % copyList)
      mesh % edgeSignOnVertex % ioinfo % input = .false.
      mesh % edgeSignOnVertex % ioinfo % sfc = .false.
      mesh % edgeSignOnVertex % ioinfo % restart = .false.
      mesh % edgeSignOnVertex % ioinfo % output = .false.
      mesh % edgeSignOnVertex % block => b

      allocate(mesh % kiteIndexOnCell)
      allocate(mesh % kiteIndexOnCell % ioinfo)
      mesh % kiteIndexOnCell % fieldName = 'kiteIndexOnCell'
      mesh % kiteIndexOnCell % isVarArray = .false.
      mesh % kiteIndexOnCell % isPersistent = .true.
      mesh % kiteIndexOnCell % isActive = .true.
      allocate(mesh % kiteIndexOnCell % array(maxEdges, nCells + 1))
      mesh % kiteIndexOnCell % array = 0
      mesh % kiteIndexOnCell % dimSizes(1) = maxEdges
      mesh % kiteIndexOnCell % dimNames(1) = 'maxEdges'
      mesh % kiteIndexOnCell % dimSizes(2) = nCells
      mesh % kiteIndexOnCell % dimNames(2) = 'nCells'
      mesh % kiteIndexOnCell % hasTimeDimension = .false.
      nullify(mesh % kiteIndexOnCell % prev)
      nullify(mesh % kiteIndexOnCell % next)
      nullify(mesh % kiteIndexOnCell % sendList)
      nullify(mesh % kiteIndexOnCell % recvList)
      nullify(mesh % kiteIndexOnCell % copyList)
      mesh % kiteIndexOnCell % ioinfo % input = .false.
      mesh % kiteIndexOnCell % ioinfo % sfc = .false.
      mesh % kiteIndexOnCell % ioinfo % restart = .false.
      mesh % kiteIndexOnCell % ioinfo % output = .false.
      mesh % kiteIndexOnCell % block => b

   end subroutine mpas_allocate_mesh


   subroutine mpas_allocate_verticalMesh(b, verticalMesh, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (verticalMesh_type), intent(inout) :: verticalMesh
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      verticalMesh % block => b
      allocate(verticalMesh % restingThickness)
      allocate(verticalMesh % restingThickness % ioinfo)
      verticalMesh % restingThickness % fieldName = 'restingThickness'
      verticalMesh % restingThickness % isVarArray = .false.
      verticalMesh % restingThickness % isPersistent = .true.
      verticalMesh % restingThickness % isActive = .true.
      allocate(verticalMesh % restingThickness % array(nVertLevels, nCells + 1))
      verticalMesh % restingThickness % array = 0.0_RKIND
      verticalMesh % restingThickness % dimSizes(1) = nVertLevels
      verticalMesh % restingThickness % dimNames(1) = 'nVertLevels'
      verticalMesh % restingThickness % dimSizes(2) = nCells
      verticalMesh % restingThickness % dimNames(2) = 'nCells'
      verticalMesh % restingThickness % hasTimeDimension = .false.
      nullify(verticalMesh % restingThickness % prev)
      nullify(verticalMesh % restingThickness % next)
      nullify(verticalMesh % restingThickness % sendList)
      nullify(verticalMesh % restingThickness % recvList)
      nullify(verticalMesh % restingThickness % copyList)
      verticalMesh % restingThickness % ioinfo % input = .true.
      verticalMesh % restingThickness % ioinfo % sfc = .false.
      verticalMesh % restingThickness % ioinfo % restart = .true.
      verticalMesh % restingThickness % ioinfo % output = .false.
      verticalMesh % restingThickness % block => b

      allocate(verticalMesh % refZMid)
      allocate(verticalMesh % refZMid % ioinfo)
      verticalMesh % refZMid % fieldName = 'refZMid'
      verticalMesh % refZMid % isVarArray = .false.
      verticalMesh % refZMid % isPersistent = .true.
      verticalMesh % refZMid % isActive = .true.
      allocate(verticalMesh % refZMid % array(nVertLevels))
      verticalMesh % refZMid % array = 0.0_RKIND
      verticalMesh % refZMid % dimSizes(1) = nVertLevels
      verticalMesh % refZMid % dimNames(1) = 'nVertLevels'
      verticalMesh % refZMid % hasTimeDimension = .false.
      nullify(verticalMesh % refZMid % prev)
      nullify(verticalMesh % refZMid % next)
      nullify(verticalMesh % refZMid % sendList)
      nullify(verticalMesh % refZMid % recvList)
      nullify(verticalMesh % refZMid % copyList)
      verticalMesh % refZMid % ioinfo % input = .false.
      verticalMesh % refZMid % ioinfo % sfc = .false.
      verticalMesh % refZMid % ioinfo % restart = .false.
      verticalMesh % refZMid % ioinfo % output = .true.
      verticalMesh % refZMid % block => b

      allocate(verticalMesh % refLayerThickness)
      allocate(verticalMesh % refLayerThickness % ioinfo)
      verticalMesh % refLayerThickness % fieldName = 'refLayerThickness'
      verticalMesh % refLayerThickness % isVarArray = .false.
      verticalMesh % refLayerThickness % isPersistent = .true.
      verticalMesh % refLayerThickness % isActive = .true.
      allocate(verticalMesh % refLayerThickness % array(nVertLevels))
      verticalMesh % refLayerThickness % array = 0.0_RKIND
      verticalMesh % refLayerThickness % dimSizes(1) = nVertLevels
      verticalMesh % refLayerThickness % dimNames(1) = 'nVertLevels'
      verticalMesh % refLayerThickness % hasTimeDimension = .false.
      nullify(verticalMesh % refLayerThickness % prev)
      nullify(verticalMesh % refLayerThickness % next)
      nullify(verticalMesh % refLayerThickness % sendList)
      nullify(verticalMesh % refLayerThickness % recvList)
      nullify(verticalMesh % refLayerThickness % copyList)
      verticalMesh % refLayerThickness % ioinfo % input = .false.
      verticalMesh % refLayerThickness % ioinfo % sfc = .false.
      verticalMesh % refLayerThickness % ioinfo % restart = .false.
      verticalMesh % refLayerThickness % ioinfo % output = .true.
      verticalMesh % refLayerThickness % block => b

   end subroutine mpas_allocate_verticalMesh


   subroutine mpas_allocate_tend(b, tend, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (tend_type), intent(inout) :: tend
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      tend % block => b
      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(tend % tracers)
      allocate(tend % tracers % ioinfo)
      tend % tracers % fieldName = 'tracers'
      tend % tracers % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      tend % index_temperature = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      tend % index_salinity = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      tend % index_tracer1 = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         tend % dynamics_start = group_start
         tend % dynamics_end = group_start + group_counter
      end if
      tend % num_tracers = index_counter
      if ( tend % num_tracers > 0 ) then
         allocate(tend % tracers % constituentNames(tend % num_tracers))
      end if
      tend % tracers % constituentNames(tend % index_temperature) = 'tendTemperature'
      tend % tracers % constituentNames(tend % index_salinity) = 'tendSalinity'
      tend % tracers % constituentNames(tend % index_tracer1) = 'tendTracer1'
      tend % tracers % isPersistent = .true.
      tend % tracers % isActive = .true.
      allocate(tend % tracers % array(tend % num_tracers, nVertLevels, nCells + 1))
      tend % tracers % array = 0.0_RKIND
      tend % tracers % dimSizes(1) = tend % num_tracers
      tend % tracers % dimNames(1) = 'num_tracers'
      tend % tracers % dimSizes(2) = nVertLevels
      tend % tracers % dimNames(2) = 'nVertLevels'
      tend % tracers % dimSizes(3) = nCells
      tend % tracers % dimNames(3) = 'nCells'
      tend % tracers % hasTimeDimension = .true.
      nullify(tend % tracers % prev)
      nullify(tend % tracers % next)
      nullify(tend % tracers % sendList)
      nullify(tend % tracers % recvList)
      nullify(tend % tracers % copyList)
      tend % tracers % ioinfo % input = .false.
      tend % tracers % ioinfo % sfc = .false.
      tend % tracers % ioinfo % restart = .false.
      tend % tracers % ioinfo % output = .false.
      tend % tracers % block => b

      allocate(tend % normalVelocity)
      allocate(tend % normalVelocity % ioinfo)
      tend % normalVelocity % fieldName = 'tendNormalVelocity'
      tend % normalVelocity % isVarArray = .false.
      tend % normalVelocity % isPersistent = .true.
      tend % normalVelocity % isActive = .true.
      allocate(tend % normalVelocity % array(nVertLevels, nEdges + 1))
      tend % normalVelocity % array = 0.0_RKIND
      tend % normalVelocity % dimSizes(1) = nVertLevels
      tend % normalVelocity % dimNames(1) = 'nVertLevels'
      tend % normalVelocity % dimSizes(2) = nEdges
      tend % normalVelocity % dimNames(2) = 'nEdges'
      tend % normalVelocity % hasTimeDimension = .true.
      nullify(tend % normalVelocity % prev)
      nullify(tend % normalVelocity % next)
      nullify(tend % normalVelocity % sendList)
      nullify(tend % normalVelocity % recvList)
      nullify(tend % normalVelocity % copyList)
      tend % normalVelocity % ioinfo % input = .false.
      tend % normalVelocity % ioinfo % sfc = .false.
      tend % normalVelocity % ioinfo % restart = .false.
      tend % normalVelocity % ioinfo % output = .false.
      tend % normalVelocity % block => b

      allocate(tend % layerThickness)
      allocate(tend % layerThickness % ioinfo)
      tend % layerThickness % fieldName = 'tendLayerThickness'
      tend % layerThickness % isVarArray = .false.
      tend % layerThickness % isPersistent = .true.
      tend % layerThickness % isActive = .true.
      allocate(tend % layerThickness % array(nVertLevels, nCells + 1))
      tend % layerThickness % array = 0.0_RKIND
      tend % layerThickness % dimSizes(1) = nVertLevels
      tend % layerThickness % dimNames(1) = 'nVertLevels'
      tend % layerThickness % dimSizes(2) = nCells
      tend % layerThickness % dimNames(2) = 'nCells'
      tend % layerThickness % hasTimeDimension = .true.
      nullify(tend % layerThickness % prev)
      nullify(tend % layerThickness % next)
      nullify(tend % layerThickness % sendList)
      nullify(tend % layerThickness % recvList)
      nullify(tend % layerThickness % copyList)
      tend % layerThickness % ioinfo % input = .false.
      tend % layerThickness % ioinfo % sfc = .false.
      tend % layerThickness % ioinfo % restart = .false.
      tend % layerThickness % ioinfo % output = .false.
      tend % layerThickness % block => b

      allocate(tend % ssh)
      allocate(tend % ssh % ioinfo)
      tend % ssh % fieldName = 'tendSsh'
      tend % ssh % isVarArray = .false.
      tend % ssh % isPersistent = .true.
      tend % ssh % isActive = .true.
      allocate(tend % ssh % array(nCells + 1))
      tend % ssh % array = 0.0_RKIND
      tend % ssh % dimSizes(1) = nCells
      tend % ssh % dimNames(1) = 'nCells'
      tend % ssh % hasTimeDimension = .true.
      nullify(tend % ssh % prev)
      nullify(tend % ssh % next)
      nullify(tend % ssh % sendList)
      nullify(tend % ssh % recvList)
      nullify(tend % ssh % copyList)
      tend % ssh % ioinfo % input = .false.
      tend % ssh % ioinfo % sfc = .false.
      tend % ssh % ioinfo % restart = .false.
      tend % ssh % ioinfo % output = .false.
      tend % ssh % block => b

      allocate(tend % highFreqThickness)
      allocate(tend % highFreqThickness % ioinfo)
      tend % highFreqThickness % fieldName = 'tendHighFreqThickness'
      tend % highFreqThickness % isVarArray = .false.
      tend % highFreqThickness % isPersistent = .true.
      tend % highFreqThickness % isActive = .true.
      allocate(tend % highFreqThickness % array(nVertLevels, nCells + 1))
      tend % highFreqThickness % array = 0.0_RKIND
      tend % highFreqThickness % dimSizes(1) = nVertLevels
      tend % highFreqThickness % dimNames(1) = 'nVertLevels'
      tend % highFreqThickness % dimSizes(2) = nCells
      tend % highFreqThickness % dimNames(2) = 'nCells'
      tend % highFreqThickness % hasTimeDimension = .true.
      nullify(tend % highFreqThickness % prev)
      nullify(tend % highFreqThickness % next)
      nullify(tend % highFreqThickness % sendList)
      nullify(tend % highFreqThickness % recvList)
      nullify(tend % highFreqThickness % copyList)
      tend % highFreqThickness % ioinfo % input = .false.
      tend % highFreqThickness % ioinfo % sfc = .false.
      tend % highFreqThickness % ioinfo % restart = .false.
      tend % highFreqThickness % ioinfo % output = .false.
      tend % highFreqThickness % block => b

      allocate(tend % lowFreqDivergence)
      allocate(tend % lowFreqDivergence % ioinfo)
      tend % lowFreqDivergence % fieldName = 'tendLowFreqDivergence'
      tend % lowFreqDivergence % isVarArray = .false.
      tend % lowFreqDivergence % isPersistent = .true.
      tend % lowFreqDivergence % isActive = .true.
      allocate(tend % lowFreqDivergence % array(nVertLevels, nCells + 1))
      tend % lowFreqDivergence % array = 0.0_RKIND
      tend % lowFreqDivergence % dimSizes(1) = nVertLevels
      tend % lowFreqDivergence % dimNames(1) = 'nVertLevels'
      tend % lowFreqDivergence % dimSizes(2) = nCells
      tend % lowFreqDivergence % dimNames(2) = 'nCells'
      tend % lowFreqDivergence % hasTimeDimension = .true.
      nullify(tend % lowFreqDivergence % prev)
      nullify(tend % lowFreqDivergence % next)
      nullify(tend % lowFreqDivergence % sendList)
      nullify(tend % lowFreqDivergence % recvList)
      nullify(tend % lowFreqDivergence % copyList)
      tend % lowFreqDivergence % ioinfo % input = .false.
      tend % lowFreqDivergence % ioinfo % sfc = .false.
      tend % lowFreqDivergence % ioinfo % restart = .false.
      tend % lowFreqDivergence % ioinfo % output = .false.
      tend % lowFreqDivergence % block => b

   end subroutine mpas_allocate_tend


   subroutine mpas_allocate_diagnostics(b, diagnostics, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (diagnostics_type), intent(inout) :: diagnostics
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      diagnostics % block => b
      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(diagnostics % tracersSurfaceValue)
      allocate(diagnostics % tracersSurfaceValue % ioinfo)
      diagnostics % tracersSurfaceValue % fieldName = 'tracersSurfaceValue'
      diagnostics % tracersSurfaceValue % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      diagnostics % index_temperatureSurfaceValue = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      diagnostics % index_salinitySurfaceValue = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      diagnostics % index_tracer1SurfaceValue = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         diagnostics % surfaceValues_start = group_start
         diagnostics % surfaceValues_end = group_start + group_counter
      end if
      diagnostics % num_tracersSurfaceValue = index_counter
      if ( diagnostics % num_tracersSurfaceValue > 0 ) then
         allocate(diagnostics % tracersSurfaceValue % constituentNames(diagnostics % num_tracersSurfaceValue))
      end if
      diagnostics % tracersSurfaceValue % constituentNames(diagnostics % index_temperatureSurfaceValue) = 'temperatureSurfaceValue'
      diagnostics % tracersSurfaceValue % constituentNames(diagnostics % index_salinitySurfaceValue) = 'salinitySurfaceValue'
      diagnostics % tracersSurfaceValue % constituentNames(diagnostics % index_tracer1SurfaceValue) = 'tracer1SurfaceValue'
      diagnostics % tracersSurfaceValue % isPersistent = .true.
      diagnostics % tracersSurfaceValue % isActive = .true.
      allocate(diagnostics % tracersSurfaceValue % array(diagnostics % num_tracersSurfaceValue, nCells + 1))
      diagnostics % tracersSurfaceValue % array = 0.0_RKIND
      diagnostics % tracersSurfaceValue % dimSizes(1) = diagnostics % num_tracersSurfaceValue
      diagnostics % tracersSurfaceValue % dimNames(1) = 'num_tracersSurfaceValue'
      diagnostics % tracersSurfaceValue % dimSizes(2) = nCells
      diagnostics % tracersSurfaceValue % dimNames(2) = 'nCells'
      diagnostics % tracersSurfaceValue % hasTimeDimension = .true.
      nullify(diagnostics % tracersSurfaceValue % prev)
      nullify(diagnostics % tracersSurfaceValue % next)
      nullify(diagnostics % tracersSurfaceValue % sendList)
      nullify(diagnostics % tracersSurfaceValue % recvList)
      nullify(diagnostics % tracersSurfaceValue % copyList)
      diagnostics % tracersSurfaceValue % ioinfo % input = .false.
      diagnostics % tracersSurfaceValue % ioinfo % sfc = .false.
      diagnostics % tracersSurfaceValue % ioinfo % restart = .false.
      diagnostics % tracersSurfaceValue % ioinfo % output = .true.
      diagnostics % tracersSurfaceValue % block => b

      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(diagnostics % surfaceVelocity)
      allocate(diagnostics % surfaceVelocity % ioinfo)
      diagnostics % surfaceVelocity % fieldName = 'surfaceVelocity'
      diagnostics % surfaceVelocity % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      diagnostics % index_zonalSurfaceVelocity = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter >= 0) then
         diagnostics % vel_zonal_start = group_start
         diagnostics % vel_zonal_end = group_start + group_counter
      end if
      group_counter = -1
      group_started = .false.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      diagnostics % index_meridionalSurfaceVelocity = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         diagnostics % vel_meridional_start = group_start
         diagnostics % vel_meridional_end = group_start + group_counter
      end if
      diagnostics % num_surfaceVelocity = index_counter
      if ( diagnostics % num_surfaceVelocity > 0 ) then
         allocate(diagnostics % surfaceVelocity % constituentNames(diagnostics % num_surfaceVelocity))
      end if
      diagnostics % surfaceVelocity % constituentNames(diagnostics % index_zonalSurfaceVelocity) = 'zonalSurfaceVelocity'
      diagnostics % surfaceVelocity % constituentNames(diagnostics % index_meridionalSurfaceVelocity) = 'meridionalSurfaceVelocity'
      diagnostics % surfaceVelocity % isPersistent = .true.
      diagnostics % surfaceVelocity % isActive = .true.
      allocate(diagnostics % surfaceVelocity % array(diagnostics % num_surfaceVelocity, nCells + 1))
      diagnostics % surfaceVelocity % array = 0.0_RKIND
      diagnostics % surfaceVelocity % dimSizes(1) = diagnostics % num_surfaceVelocity
      diagnostics % surfaceVelocity % dimNames(1) = 'num_surfaceVelocity'
      diagnostics % surfaceVelocity % dimSizes(2) = nCells
      diagnostics % surfaceVelocity % dimNames(2) = 'nCells'
      diagnostics % surfaceVelocity % hasTimeDimension = .true.
      nullify(diagnostics % surfaceVelocity % prev)
      nullify(diagnostics % surfaceVelocity % next)
      nullify(diagnostics % surfaceVelocity % sendList)
      nullify(diagnostics % surfaceVelocity % recvList)
      nullify(diagnostics % surfaceVelocity % copyList)
      diagnostics % surfaceVelocity % ioinfo % input = .false.
      diagnostics % surfaceVelocity % ioinfo % sfc = .false.
      diagnostics % surfaceVelocity % ioinfo % restart = .false.
      diagnostics % surfaceVelocity % ioinfo % output = .true.
      diagnostics % surfaceVelocity % block => b

      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(diagnostics % SSHGradient)
      allocate(diagnostics % SSHGradient % ioinfo)
      diagnostics % SSHGradient % fieldName = 'SSHGradient'
      diagnostics % SSHGradient % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      diagnostics % index_zonalSSHGradient = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter >= 0) then
         diagnostics % ssh_zonal_start = group_start
         diagnostics % ssh_zonal_end = group_start + group_counter
      end if
      group_counter = -1
      group_started = .false.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      diagnostics % index_meridionalSSHGradient = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         diagnostics % ssh_meridional_start = group_start
         diagnostics % ssh_meridional_end = group_start + group_counter
      end if
      diagnostics % num_SSHGradient = index_counter
      if ( diagnostics % num_SSHGradient > 0 ) then
         allocate(diagnostics % SSHGradient % constituentNames(diagnostics % num_SSHGradient))
      end if
      diagnostics % SSHGradient % constituentNames(diagnostics % index_zonalSSHGradient) = 'zonalSSHGradient'
      diagnostics % SSHGradient % constituentNames(diagnostics % index_meridionalSSHGradient) = 'meridionalSSHGradient'
      diagnostics % SSHGradient % isPersistent = .true.
      diagnostics % SSHGradient % isActive = .true.
      allocate(diagnostics % SSHGradient % array(diagnostics % num_SSHGradient, nCells + 1))
      diagnostics % SSHGradient % array = 0.0_RKIND
      diagnostics % SSHGradient % dimSizes(1) = diagnostics % num_SSHGradient
      diagnostics % SSHGradient % dimNames(1) = 'num_SSHGradient'
      diagnostics % SSHGradient % dimSizes(2) = nCells
      diagnostics % SSHGradient % dimNames(2) = 'nCells'
      diagnostics % SSHGradient % hasTimeDimension = .true.
      nullify(diagnostics % SSHGradient % prev)
      nullify(diagnostics % SSHGradient % next)
      nullify(diagnostics % SSHGradient % sendList)
      nullify(diagnostics % SSHGradient % recvList)
      nullify(diagnostics % SSHGradient % copyList)
      diagnostics % SSHGradient % ioinfo % input = .false.
      diagnostics % SSHGradient % ioinfo % sfc = .false.
      diagnostics % SSHGradient % ioinfo % restart = .false.
      diagnostics % SSHGradient % ioinfo % output = .true.
      diagnostics % SSHGradient % block => b

      allocate(diagnostics % xtime)
      allocate(diagnostics % xtime % ioinfo)
      diagnostics % xtime % fieldName = 'xtime'
      diagnostics % xtime % isVarArray = .false.
      diagnostics % xtime % hasTimeDimension = .true.
      nullify(diagnostics % xtime % prev)
      nullify(diagnostics % xtime % next)
      nullify(diagnostics % xtime % sendList)
      nullify(diagnostics % xtime % recvList)
      nullify(diagnostics % xtime % copyList)
      diagnostics % xtime % ioinfo % input = .false.
      diagnostics % xtime % ioinfo % sfc = .false.
      diagnostics % xtime % ioinfo % restart = .true.
      diagnostics % xtime % ioinfo % output = .true.
      diagnostics % xtime % block => b

      allocate(diagnostics % zMid)
      allocate(diagnostics % zMid % ioinfo)
      diagnostics % zMid % fieldName = 'zMid'
      diagnostics % zMid % isVarArray = .false.
      diagnostics % zMid % isPersistent = .true.
      diagnostics % zMid % isActive = .true.
      allocate(diagnostics % zMid % array(nVertLevels, nCells + 1))
      diagnostics % zMid % array = 0.0_RKIND
      diagnostics % zMid % dimSizes(1) = nVertLevels
      diagnostics % zMid % dimNames(1) = 'nVertLevels'
      diagnostics % zMid % dimSizes(2) = nCells
      diagnostics % zMid % dimNames(2) = 'nCells'
      diagnostics % zMid % hasTimeDimension = .true.
      nullify(diagnostics % zMid % prev)
      nullify(diagnostics % zMid % next)
      nullify(diagnostics % zMid % sendList)
      nullify(diagnostics % zMid % recvList)
      nullify(diagnostics % zMid % copyList)
      diagnostics % zMid % ioinfo % input = .false.
      diagnostics % zMid % ioinfo % sfc = .false.
      diagnostics % zMid % ioinfo % restart = .false.
      diagnostics % zMid % ioinfo % output = .true.
      diagnostics % zMid % block => b

      allocate(diagnostics % zTop)
      allocate(diagnostics % zTop % ioinfo)
      diagnostics % zTop % fieldName = 'zTop'
      diagnostics % zTop % isVarArray = .false.
      diagnostics % zTop % isPersistent = .true.
      diagnostics % zTop % isActive = .true.
      allocate(diagnostics % zTop % array(nVertLevels, nCells + 1))
      diagnostics % zTop % array = 0.0_RKIND
      diagnostics % zTop % dimSizes(1) = nVertLevels
      diagnostics % zTop % dimNames(1) = 'nVertLevels'
      diagnostics % zTop % dimSizes(2) = nCells
      diagnostics % zTop % dimNames(2) = 'nCells'
      diagnostics % zTop % hasTimeDimension = .true.
      nullify(diagnostics % zTop % prev)
      nullify(diagnostics % zTop % next)
      nullify(diagnostics % zTop % sendList)
      nullify(diagnostics % zTop % recvList)
      nullify(diagnostics % zTop % copyList)
      diagnostics % zTop % ioinfo % input = .false.
      diagnostics % zTop % ioinfo % sfc = .false.
      diagnostics % zTop % ioinfo % restart = .false.
      diagnostics % zTop % ioinfo % output = .true.
      diagnostics % zTop % block => b

      allocate(diagnostics % density)
      allocate(diagnostics % density % ioinfo)
      diagnostics % density % fieldName = 'density'
      diagnostics % density % isVarArray = .false.
      diagnostics % density % isPersistent = .true.
      diagnostics % density % isActive = .true.
      allocate(diagnostics % density % array(nVertLevels, nCells + 1))
      diagnostics % density % array = 0.0_RKIND
      diagnostics % density % dimSizes(1) = nVertLevels
      diagnostics % density % dimNames(1) = 'nVertLevels'
      diagnostics % density % dimSizes(2) = nCells
      diagnostics % density % dimNames(2) = 'nCells'
      diagnostics % density % hasTimeDimension = .true.
      nullify(diagnostics % density % prev)
      nullify(diagnostics % density % next)
      nullify(diagnostics % density % sendList)
      nullify(diagnostics % density % recvList)
      nullify(diagnostics % density % copyList)
      diagnostics % density % ioinfo % input = .false.
      diagnostics % density % ioinfo % sfc = .false.
      diagnostics % density % ioinfo % restart = .false.
      diagnostics % density % ioinfo % output = .true.
      diagnostics % density % block => b

      allocate(diagnostics % displacedDensity)
      allocate(diagnostics % displacedDensity % ioinfo)
      diagnostics % displacedDensity % fieldName = 'displacedDensity'
      diagnostics % displacedDensity % isVarArray = .false.
      diagnostics % displacedDensity % isPersistent = .true.
      diagnostics % displacedDensity % isActive = .true.
      allocate(diagnostics % displacedDensity % array(nVertLevels, nCells + 1))
      diagnostics % displacedDensity % array = 0.0_RKIND
      diagnostics % displacedDensity % dimSizes(1) = nVertLevels
      diagnostics % displacedDensity % dimNames(1) = 'nVertLevels'
      diagnostics % displacedDensity % dimSizes(2) = nCells
      diagnostics % displacedDensity % dimNames(2) = 'nCells'
      diagnostics % displacedDensity % hasTimeDimension = .true.
      nullify(diagnostics % displacedDensity % prev)
      nullify(diagnostics % displacedDensity % next)
      nullify(diagnostics % displacedDensity % sendList)
      nullify(diagnostics % displacedDensity % recvList)
      nullify(diagnostics % displacedDensity % copyList)
      diagnostics % displacedDensity % ioinfo % input = .false.
      diagnostics % displacedDensity % ioinfo % sfc = .false.
      diagnostics % displacedDensity % ioinfo % restart = .false.
      diagnostics % displacedDensity % ioinfo % output = .true.
      diagnostics % displacedDensity % block => b

      allocate(diagnostics % potentialDensity)
      allocate(diagnostics % potentialDensity % ioinfo)
      diagnostics % potentialDensity % fieldName = 'potentialDensity'
      diagnostics % potentialDensity % isVarArray = .false.
      diagnostics % potentialDensity % isPersistent = .true.
      diagnostics % potentialDensity % isActive = .true.
      allocate(diagnostics % potentialDensity % array(nVertLevels, nCells + 1))
      diagnostics % potentialDensity % array = 0.0_RKIND
      diagnostics % potentialDensity % dimSizes(1) = nVertLevels
      diagnostics % potentialDensity % dimNames(1) = 'nVertLevels'
      diagnostics % potentialDensity % dimSizes(2) = nCells
      diagnostics % potentialDensity % dimNames(2) = 'nCells'
      diagnostics % potentialDensity % hasTimeDimension = .true.
      nullify(diagnostics % potentialDensity % prev)
      nullify(diagnostics % potentialDensity % next)
      nullify(diagnostics % potentialDensity % sendList)
      nullify(diagnostics % potentialDensity % recvList)
      nullify(diagnostics % potentialDensity % copyList)
      diagnostics % potentialDensity % ioinfo % input = .false.
      diagnostics % potentialDensity % ioinfo % sfc = .false.
      diagnostics % potentialDensity % ioinfo % restart = .false.
      diagnostics % potentialDensity % ioinfo % output = .true.
      diagnostics % potentialDensity % block => b

      allocate(diagnostics % BruntVaisalaFreqTop)
      allocate(diagnostics % BruntVaisalaFreqTop % ioinfo)
      diagnostics % BruntVaisalaFreqTop % fieldName = 'BruntVaisalaFreqTop'
      diagnostics % BruntVaisalaFreqTop % isVarArray = .false.
      diagnostics % BruntVaisalaFreqTop % isPersistent = .true.
      diagnostics % BruntVaisalaFreqTop % isActive = .true.
      allocate(diagnostics % BruntVaisalaFreqTop % array(nVertLevels, nCells + 1))
      diagnostics % BruntVaisalaFreqTop % array = 0.0_RKIND
      diagnostics % BruntVaisalaFreqTop % dimSizes(1) = nVertLevels
      diagnostics % BruntVaisalaFreqTop % dimNames(1) = 'nVertLevels'
      diagnostics % BruntVaisalaFreqTop % dimSizes(2) = nCells
      diagnostics % BruntVaisalaFreqTop % dimNames(2) = 'nCells'
      diagnostics % BruntVaisalaFreqTop % hasTimeDimension = .true.
      nullify(diagnostics % BruntVaisalaFreqTop % prev)
      nullify(diagnostics % BruntVaisalaFreqTop % next)
      nullify(diagnostics % BruntVaisalaFreqTop % sendList)
      nullify(diagnostics % BruntVaisalaFreqTop % recvList)
      nullify(diagnostics % BruntVaisalaFreqTop % copyList)
      diagnostics % BruntVaisalaFreqTop % ioinfo % input = .false.
      diagnostics % BruntVaisalaFreqTop % ioinfo % sfc = .false.
      diagnostics % BruntVaisalaFreqTop % ioinfo % restart = .false.
      diagnostics % BruntVaisalaFreqTop % ioinfo % output = .true.
      diagnostics % BruntVaisalaFreqTop % block => b

      allocate(diagnostics % montgomeryPotential)
      allocate(diagnostics % montgomeryPotential % ioinfo)
      diagnostics % montgomeryPotential % fieldName = 'montgomeryPotential'
      diagnostics % montgomeryPotential % isVarArray = .false.
      diagnostics % montgomeryPotential % isPersistent = .true.
      diagnostics % montgomeryPotential % isActive = .true.
      allocate(diagnostics % montgomeryPotential % array(nVertLevels, nCells + 1))
      diagnostics % montgomeryPotential % array = 0.0_RKIND
      diagnostics % montgomeryPotential % dimSizes(1) = nVertLevels
      diagnostics % montgomeryPotential % dimNames(1) = 'nVertLevels'
      diagnostics % montgomeryPotential % dimSizes(2) = nCells
      diagnostics % montgomeryPotential % dimNames(2) = 'nCells'
      diagnostics % montgomeryPotential % hasTimeDimension = .true.
      nullify(diagnostics % montgomeryPotential % prev)
      nullify(diagnostics % montgomeryPotential % next)
      nullify(diagnostics % montgomeryPotential % sendList)
      nullify(diagnostics % montgomeryPotential % recvList)
      nullify(diagnostics % montgomeryPotential % copyList)
      diagnostics % montgomeryPotential % ioinfo % input = .false.
      diagnostics % montgomeryPotential % ioinfo % sfc = .false.
      diagnostics % montgomeryPotential % ioinfo % restart = .false.
      diagnostics % montgomeryPotential % ioinfo % output = .false.
      diagnostics % montgomeryPotential % block => b

      allocate(diagnostics % pressure)
      allocate(diagnostics % pressure % ioinfo)
      diagnostics % pressure % fieldName = 'pressure'
      diagnostics % pressure % isVarArray = .false.
      diagnostics % pressure % isPersistent = .true.
      diagnostics % pressure % isActive = .true.
      allocate(diagnostics % pressure % array(nVertLevels, nCells + 1))
      diagnostics % pressure % array = 0.0_RKIND
      diagnostics % pressure % dimSizes(1) = nVertLevels
      diagnostics % pressure % dimNames(1) = 'nVertLevels'
      diagnostics % pressure % dimSizes(2) = nCells
      diagnostics % pressure % dimNames(2) = 'nCells'
      diagnostics % pressure % hasTimeDimension = .true.
      nullify(diagnostics % pressure % prev)
      nullify(diagnostics % pressure % next)
      nullify(diagnostics % pressure % sendList)
      nullify(diagnostics % pressure % recvList)
      nullify(diagnostics % pressure % copyList)
      diagnostics % pressure % ioinfo % input = .false.
      diagnostics % pressure % ioinfo % sfc = .false.
      diagnostics % pressure % ioinfo % restart = .false.
      diagnostics % pressure % ioinfo % output = .true.
      diagnostics % pressure % block => b

      allocate(diagnostics % uTransport)
      allocate(diagnostics % uTransport % ioinfo)
      diagnostics % uTransport % fieldName = 'uTransport'
      diagnostics % uTransport % isVarArray = .false.
      diagnostics % uTransport % isPersistent = .true.
      diagnostics % uTransport % isActive = .true.
      allocate(diagnostics % uTransport % array(nVertLevels, nEdges + 1))
      diagnostics % uTransport % array = 0.0_RKIND
      diagnostics % uTransport % dimSizes(1) = nVertLevels
      diagnostics % uTransport % dimNames(1) = 'nVertLevels'
      diagnostics % uTransport % dimSizes(2) = nEdges
      diagnostics % uTransport % dimNames(2) = 'nEdges'
      diagnostics % uTransport % hasTimeDimension = .true.
      nullify(diagnostics % uTransport % prev)
      nullify(diagnostics % uTransport % next)
      nullify(diagnostics % uTransport % sendList)
      nullify(diagnostics % uTransport % recvList)
      nullify(diagnostics % uTransport % copyList)
      diagnostics % uTransport % ioinfo % input = .false.
      diagnostics % uTransport % ioinfo % sfc = .false.
      diagnostics % uTransport % ioinfo % restart = .false.
      diagnostics % uTransport % ioinfo % output = .false.
      diagnostics % uTransport % block => b

      allocate(diagnostics % vertTransportVelocityTop)
      allocate(diagnostics % vertTransportVelocityTop % ioinfo)
      diagnostics % vertTransportVelocityTop % fieldName = 'vertTransportVelocityTop'
      diagnostics % vertTransportVelocityTop % isVarArray = .false.
      diagnostics % vertTransportVelocityTop % isPersistent = .true.
      diagnostics % vertTransportVelocityTop % isActive = .true.
      allocate(diagnostics % vertTransportVelocityTop % array(nVertLevels+1, nCells + 1))
      diagnostics % vertTransportVelocityTop % array = 0.0_RKIND
      diagnostics % vertTransportVelocityTop % dimSizes(1) = nVertLevels+1
      diagnostics % vertTransportVelocityTop % dimNames(1) = 'nVertLevelsP1'
      diagnostics % vertTransportVelocityTop % dimSizes(2) = nCells
      diagnostics % vertTransportVelocityTop % dimNames(2) = 'nCells'
      diagnostics % vertTransportVelocityTop % hasTimeDimension = .true.
      nullify(diagnostics % vertTransportVelocityTop % prev)
      nullify(diagnostics % vertTransportVelocityTop % next)
      nullify(diagnostics % vertTransportVelocityTop % sendList)
      nullify(diagnostics % vertTransportVelocityTop % recvList)
      nullify(diagnostics % vertTransportVelocityTop % copyList)
      diagnostics % vertTransportVelocityTop % ioinfo % input = .false.
      diagnostics % vertTransportVelocityTop % ioinfo % sfc = .false.
      diagnostics % vertTransportVelocityTop % ioinfo % restart = .false.
      diagnostics % vertTransportVelocityTop % ioinfo % output = .true.
      diagnostics % vertTransportVelocityTop % block => b

      allocate(diagnostics % vertVelocityTop)
      allocate(diagnostics % vertVelocityTop % ioinfo)
      diagnostics % vertVelocityTop % fieldName = 'vertVelocityTop'
      diagnostics % vertVelocityTop % isVarArray = .false.
      diagnostics % vertVelocityTop % isPersistent = .true.
      diagnostics % vertVelocityTop % isActive = .true.
      allocate(diagnostics % vertVelocityTop % array(nVertLevels+1, nCells + 1))
      diagnostics % vertVelocityTop % array = 0.0_RKIND
      diagnostics % vertVelocityTop % dimSizes(1) = nVertLevels+1
      diagnostics % vertVelocityTop % dimNames(1) = 'nVertLevelsP1'
      diagnostics % vertVelocityTop % dimSizes(2) = nCells
      diagnostics % vertVelocityTop % dimNames(2) = 'nCells'
      diagnostics % vertVelocityTop % hasTimeDimension = .true.
      nullify(diagnostics % vertVelocityTop % prev)
      nullify(diagnostics % vertVelocityTop % next)
      nullify(diagnostics % vertVelocityTop % sendList)
      nullify(diagnostics % vertVelocityTop % recvList)
      nullify(diagnostics % vertVelocityTop % copyList)
      diagnostics % vertVelocityTop % ioinfo % input = .false.
      diagnostics % vertVelocityTop % ioinfo % sfc = .false.
      diagnostics % vertVelocityTop % ioinfo % restart = .false.
      diagnostics % vertVelocityTop % ioinfo % output = .true.
      diagnostics % vertVelocityTop % block => b

      allocate(diagnostics % tangentialVelocity)
      allocate(diagnostics % tangentialVelocity % ioinfo)
      diagnostics % tangentialVelocity % fieldName = 'tangentialVelocity'
      diagnostics % tangentialVelocity % isVarArray = .false.
      diagnostics % tangentialVelocity % isPersistent = .true.
      diagnostics % tangentialVelocity % isActive = .true.
      allocate(diagnostics % tangentialVelocity % array(nVertLevels, nEdges + 1))
      diagnostics % tangentialVelocity % array = 0.0_RKIND
      diagnostics % tangentialVelocity % dimSizes(1) = nVertLevels
      diagnostics % tangentialVelocity % dimNames(1) = 'nVertLevels'
      diagnostics % tangentialVelocity % dimSizes(2) = nEdges
      diagnostics % tangentialVelocity % dimNames(2) = 'nEdges'
      diagnostics % tangentialVelocity % hasTimeDimension = .true.
      nullify(diagnostics % tangentialVelocity % prev)
      nullify(diagnostics % tangentialVelocity % next)
      nullify(diagnostics % tangentialVelocity % sendList)
      nullify(diagnostics % tangentialVelocity % recvList)
      nullify(diagnostics % tangentialVelocity % copyList)
      diagnostics % tangentialVelocity % ioinfo % input = .false.
      diagnostics % tangentialVelocity % ioinfo % sfc = .false.
      diagnostics % tangentialVelocity % ioinfo % restart = .false.
      diagnostics % tangentialVelocity % ioinfo % output = .false.
      diagnostics % tangentialVelocity % block => b

      allocate(diagnostics % layerThicknessEdge)
      allocate(diagnostics % layerThicknessEdge % ioinfo)
      diagnostics % layerThicknessEdge % fieldName = 'layerThicknessEdge'
      diagnostics % layerThicknessEdge % isVarArray = .false.
      diagnostics % layerThicknessEdge % isPersistent = .true.
      diagnostics % layerThicknessEdge % isActive = .true.
      allocate(diagnostics % layerThicknessEdge % array(nVertLevels, nEdges + 1))
      diagnostics % layerThicknessEdge % array = 0.0_RKIND
      diagnostics % layerThicknessEdge % dimSizes(1) = nVertLevels
      diagnostics % layerThicknessEdge % dimNames(1) = 'nVertLevels'
      diagnostics % layerThicknessEdge % dimSizes(2) = nEdges
      diagnostics % layerThicknessEdge % dimNames(2) = 'nEdges'
      diagnostics % layerThicknessEdge % hasTimeDimension = .true.
      nullify(diagnostics % layerThicknessEdge % prev)
      nullify(diagnostics % layerThicknessEdge % next)
      nullify(diagnostics % layerThicknessEdge % sendList)
      nullify(diagnostics % layerThicknessEdge % recvList)
      nullify(diagnostics % layerThicknessEdge % copyList)
      diagnostics % layerThicknessEdge % ioinfo % input = .false.
      diagnostics % layerThicknessEdge % ioinfo % sfc = .false.
      diagnostics % layerThicknessEdge % ioinfo % restart = .false.
      diagnostics % layerThicknessEdge % ioinfo % output = .false.
      diagnostics % layerThicknessEdge % block => b

      allocate(diagnostics % layerThicknessVertex)
      allocate(diagnostics % layerThicknessVertex % ioinfo)
      diagnostics % layerThicknessVertex % fieldName = 'layerThicknessVertex'
      diagnostics % layerThicknessVertex % isVarArray = .false.
      diagnostics % layerThicknessVertex % isPersistent = .true.
      diagnostics % layerThicknessVertex % isActive = .true.
      allocate(diagnostics % layerThicknessVertex % array(nVertLevels, nVertices + 1))
      diagnostics % layerThicknessVertex % array = 0.0_RKIND
      diagnostics % layerThicknessVertex % dimSizes(1) = nVertLevels
      diagnostics % layerThicknessVertex % dimNames(1) = 'nVertLevels'
      diagnostics % layerThicknessVertex % dimSizes(2) = nVertices
      diagnostics % layerThicknessVertex % dimNames(2) = 'nVertices'
      diagnostics % layerThicknessVertex % hasTimeDimension = .true.
      nullify(diagnostics % layerThicknessVertex % prev)
      nullify(diagnostics % layerThicknessVertex % next)
      nullify(diagnostics % layerThicknessVertex % sendList)
      nullify(diagnostics % layerThicknessVertex % recvList)
      nullify(diagnostics % layerThicknessVertex % copyList)
      diagnostics % layerThicknessVertex % ioinfo % input = .false.
      diagnostics % layerThicknessVertex % ioinfo % sfc = .false.
      diagnostics % layerThicknessVertex % ioinfo % restart = .false.
      diagnostics % layerThicknessVertex % ioinfo % output = .false.
      diagnostics % layerThicknessVertex % block => b

      allocate(diagnostics % kineticEnergyCell)
      allocate(diagnostics % kineticEnergyCell % ioinfo)
      diagnostics % kineticEnergyCell % fieldName = 'kineticEnergyCell'
      diagnostics % kineticEnergyCell % isVarArray = .false.
      diagnostics % kineticEnergyCell % isPersistent = .true.
      diagnostics % kineticEnergyCell % isActive = .true.
      allocate(diagnostics % kineticEnergyCell % array(nVertLevels, nCells + 1))
      diagnostics % kineticEnergyCell % array = 0.0_RKIND
      diagnostics % kineticEnergyCell % dimSizes(1) = nVertLevels
      diagnostics % kineticEnergyCell % dimNames(1) = 'nVertLevels'
      diagnostics % kineticEnergyCell % dimSizes(2) = nCells
      diagnostics % kineticEnergyCell % dimNames(2) = 'nCells'
      diagnostics % kineticEnergyCell % hasTimeDimension = .true.
      nullify(diagnostics % kineticEnergyCell % prev)
      nullify(diagnostics % kineticEnergyCell % next)
      nullify(diagnostics % kineticEnergyCell % sendList)
      nullify(diagnostics % kineticEnergyCell % recvList)
      nullify(diagnostics % kineticEnergyCell % copyList)
      diagnostics % kineticEnergyCell % ioinfo % input = .false.
      diagnostics % kineticEnergyCell % ioinfo % sfc = .false.
      diagnostics % kineticEnergyCell % ioinfo % restart = .false.
      diagnostics % kineticEnergyCell % ioinfo % output = .true.
      diagnostics % kineticEnergyCell % block => b

      allocate(diagnostics % hEddyFlux)
      allocate(diagnostics % hEddyFlux % ioinfo)
      diagnostics % hEddyFlux % fieldName = 'hEddyFlux'
      diagnostics % hEddyFlux % isVarArray = .false.
      diagnostics % hEddyFlux % isPersistent = .true.
      diagnostics % hEddyFlux % isActive = .true.
      allocate(diagnostics % hEddyFlux % array(nVertLevels, nEdges + 1))
      diagnostics % hEddyFlux % array = 0.0_RKIND
      diagnostics % hEddyFlux % dimSizes(1) = nVertLevels
      diagnostics % hEddyFlux % dimNames(1) = 'nVertLevels'
      diagnostics % hEddyFlux % dimSizes(2) = nEdges
      diagnostics % hEddyFlux % dimNames(2) = 'nEdges'
      diagnostics % hEddyFlux % hasTimeDimension = .true.
      nullify(diagnostics % hEddyFlux % prev)
      nullify(diagnostics % hEddyFlux % next)
      nullify(diagnostics % hEddyFlux % sendList)
      nullify(diagnostics % hEddyFlux % recvList)
      nullify(diagnostics % hEddyFlux % copyList)
      diagnostics % hEddyFlux % ioinfo % input = .false.
      diagnostics % hEddyFlux % ioinfo % sfc = .false.
      diagnostics % hEddyFlux % ioinfo % restart = .false.
      diagnostics % hEddyFlux % ioinfo % output = .false.
      diagnostics % hEddyFlux % block => b

      allocate(diagnostics % hKappa)
      allocate(diagnostics % hKappa % ioinfo)
      diagnostics % hKappa % fieldName = 'hKappa'
      diagnostics % hKappa % isVarArray = .false.
      diagnostics % hKappa % isPersistent = .true.
      diagnostics % hKappa % isActive = .true.
      allocate(diagnostics % hKappa % array(nVertLevels, nEdges + 1))
      diagnostics % hKappa % array = 0.0_RKIND
      diagnostics % hKappa % dimSizes(1) = nVertLevels
      diagnostics % hKappa % dimNames(1) = 'nVertLevels'
      diagnostics % hKappa % dimSizes(2) = nEdges
      diagnostics % hKappa % dimNames(2) = 'nEdges'
      diagnostics % hKappa % hasTimeDimension = .true.
      nullify(diagnostics % hKappa % prev)
      nullify(diagnostics % hKappa % next)
      nullify(diagnostics % hKappa % sendList)
      nullify(diagnostics % hKappa % recvList)
      nullify(diagnostics % hKappa % copyList)
      diagnostics % hKappa % ioinfo % input = .false.
      diagnostics % hKappa % ioinfo % sfc = .false.
      diagnostics % hKappa % ioinfo % restart = .false.
      diagnostics % hKappa % ioinfo % output = .false.
      diagnostics % hKappa % block => b

      allocate(diagnostics % hKappaQ)
      allocate(diagnostics % hKappaQ % ioinfo)
      diagnostics % hKappaQ % fieldName = 'hKappaQ'
      diagnostics % hKappaQ % isVarArray = .false.
      diagnostics % hKappaQ % isPersistent = .true.
      diagnostics % hKappaQ % isActive = .true.
      allocate(diagnostics % hKappaQ % array(nVertLevels, nEdges + 1))
      diagnostics % hKappaQ % array = 0.0_RKIND
      diagnostics % hKappaQ % dimSizes(1) = nVertLevels
      diagnostics % hKappaQ % dimNames(1) = 'nVertLevels'
      diagnostics % hKappaQ % dimSizes(2) = nEdges
      diagnostics % hKappaQ % dimNames(2) = 'nEdges'
      diagnostics % hKappaQ % hasTimeDimension = .true.
      nullify(diagnostics % hKappaQ % prev)
      nullify(diagnostics % hKappaQ % next)
      nullify(diagnostics % hKappaQ % sendList)
      nullify(diagnostics % hKappaQ % recvList)
      nullify(diagnostics % hKappaQ % copyList)
      diagnostics % hKappaQ % ioinfo % input = .false.
      diagnostics % hKappaQ % ioinfo % sfc = .false.
      diagnostics % hKappaQ % ioinfo % restart = .false.
      diagnostics % hKappaQ % ioinfo % output = .false.
      diagnostics % hKappaQ % block => b

      allocate(diagnostics % viscosity)
      allocate(diagnostics % viscosity % ioinfo)
      diagnostics % viscosity % fieldName = 'viscosity'
      diagnostics % viscosity % isVarArray = .false.
      diagnostics % viscosity % isPersistent = .true.
      diagnostics % viscosity % isActive = .true.
      allocate(diagnostics % viscosity % array(nVertLevels, nEdges + 1))
      diagnostics % viscosity % array = 0.0_RKIND
      diagnostics % viscosity % dimSizes(1) = nVertLevels
      diagnostics % viscosity % dimNames(1) = 'nVertLevels'
      diagnostics % viscosity % dimSizes(2) = nEdges
      diagnostics % viscosity % dimNames(2) = 'nEdges'
      diagnostics % viscosity % hasTimeDimension = .true.
      nullify(diagnostics % viscosity % prev)
      nullify(diagnostics % viscosity % next)
      nullify(diagnostics % viscosity % sendList)
      nullify(diagnostics % viscosity % recvList)
      nullify(diagnostics % viscosity % copyList)
      diagnostics % viscosity % ioinfo % input = .false.
      diagnostics % viscosity % ioinfo % sfc = .false.
      diagnostics % viscosity % ioinfo % restart = .false.
      diagnostics % viscosity % ioinfo % output = .true.
      diagnostics % viscosity % block => b

      allocate(diagnostics % divergence)
      allocate(diagnostics % divergence % ioinfo)
      diagnostics % divergence % fieldName = 'divergence'
      diagnostics % divergence % isVarArray = .false.
      diagnostics % divergence % isPersistent = .true.
      diagnostics % divergence % isActive = .true.
      allocate(diagnostics % divergence % array(nVertLevels, nCells + 1))
      diagnostics % divergence % array = 0.0_RKIND
      diagnostics % divergence % dimSizes(1) = nVertLevels
      diagnostics % divergence % dimNames(1) = 'nVertLevels'
      diagnostics % divergence % dimSizes(2) = nCells
      diagnostics % divergence % dimNames(2) = 'nCells'
      diagnostics % divergence % hasTimeDimension = .true.
      nullify(diagnostics % divergence % prev)
      nullify(diagnostics % divergence % next)
      nullify(diagnostics % divergence % sendList)
      nullify(diagnostics % divergence % recvList)
      nullify(diagnostics % divergence % copyList)
      diagnostics % divergence % ioinfo % input = .false.
      diagnostics % divergence % ioinfo % sfc = .false.
      diagnostics % divergence % ioinfo % restart = .false.
      diagnostics % divergence % ioinfo % output = .true.
      diagnostics % divergence % block => b

      allocate(diagnostics % circulation)
      allocate(diagnostics % circulation % ioinfo)
      diagnostics % circulation % fieldName = 'circulation'
      diagnostics % circulation % isVarArray = .false.
      diagnostics % circulation % isPersistent = .true.
      diagnostics % circulation % isActive = .true.
      allocate(diagnostics % circulation % array(nVertLevels, nVertices + 1))
      diagnostics % circulation % array = 0.0_RKIND
      diagnostics % circulation % dimSizes(1) = nVertLevels
      diagnostics % circulation % dimNames(1) = 'nVertLevels'
      diagnostics % circulation % dimSizes(2) = nVertices
      diagnostics % circulation % dimNames(2) = 'nVertices'
      diagnostics % circulation % hasTimeDimension = .true.
      nullify(diagnostics % circulation % prev)
      nullify(diagnostics % circulation % next)
      nullify(diagnostics % circulation % sendList)
      nullify(diagnostics % circulation % recvList)
      nullify(diagnostics % circulation % copyList)
      diagnostics % circulation % ioinfo % input = .false.
      diagnostics % circulation % ioinfo % sfc = .false.
      diagnostics % circulation % ioinfo % restart = .false.
      diagnostics % circulation % ioinfo % output = .false.
      diagnostics % circulation % block => b

      allocate(diagnostics % relativeVorticity)
      allocate(diagnostics % relativeVorticity % ioinfo)
      diagnostics % relativeVorticity % fieldName = 'relativeVorticity'
      diagnostics % relativeVorticity % isVarArray = .false.
      diagnostics % relativeVorticity % isPersistent = .true.
      diagnostics % relativeVorticity % isActive = .true.
      allocate(diagnostics % relativeVorticity % array(nVertLevels, nVertices + 1))
      diagnostics % relativeVorticity % array = 0.0_RKIND
      diagnostics % relativeVorticity % dimSizes(1) = nVertLevels
      diagnostics % relativeVorticity % dimNames(1) = 'nVertLevels'
      diagnostics % relativeVorticity % dimSizes(2) = nVertices
      diagnostics % relativeVorticity % dimNames(2) = 'nVertices'
      diagnostics % relativeVorticity % hasTimeDimension = .true.
      nullify(diagnostics % relativeVorticity % prev)
      nullify(diagnostics % relativeVorticity % next)
      nullify(diagnostics % relativeVorticity % sendList)
      nullify(diagnostics % relativeVorticity % recvList)
      nullify(diagnostics % relativeVorticity % copyList)
      diagnostics % relativeVorticity % ioinfo % input = .false.
      diagnostics % relativeVorticity % ioinfo % sfc = .false.
      diagnostics % relativeVorticity % ioinfo % restart = .false.
      diagnostics % relativeVorticity % ioinfo % output = .false.
      diagnostics % relativeVorticity % block => b

      allocate(diagnostics % relativeVorticityCell)
      allocate(diagnostics % relativeVorticityCell % ioinfo)
      diagnostics % relativeVorticityCell % fieldName = 'relativeVorticityCell'
      diagnostics % relativeVorticityCell % isVarArray = .false.
      diagnostics % relativeVorticityCell % isPersistent = .true.
      diagnostics % relativeVorticityCell % isActive = .true.
      allocate(diagnostics % relativeVorticityCell % array(nVertLevels, nCells + 1))
      diagnostics % relativeVorticityCell % array = 0.0_RKIND
      diagnostics % relativeVorticityCell % dimSizes(1) = nVertLevels
      diagnostics % relativeVorticityCell % dimNames(1) = 'nVertLevels'
      diagnostics % relativeVorticityCell % dimSizes(2) = nCells
      diagnostics % relativeVorticityCell % dimNames(2) = 'nCells'
      diagnostics % relativeVorticityCell % hasTimeDimension = .true.
      nullify(diagnostics % relativeVorticityCell % prev)
      nullify(diagnostics % relativeVorticityCell % next)
      nullify(diagnostics % relativeVorticityCell % sendList)
      nullify(diagnostics % relativeVorticityCell % recvList)
      nullify(diagnostics % relativeVorticityCell % copyList)
      diagnostics % relativeVorticityCell % ioinfo % input = .false.
      diagnostics % relativeVorticityCell % ioinfo % sfc = .false.
      diagnostics % relativeVorticityCell % ioinfo % restart = .false.
      diagnostics % relativeVorticityCell % ioinfo % output = .true.
      diagnostics % relativeVorticityCell % block => b

      allocate(diagnostics % normalizedRelativeVorticityEdge)
      allocate(diagnostics % normalizedRelativeVorticityEdge % ioinfo)
      diagnostics % normalizedRelativeVorticityEdge % fieldName = 'normalizedRelativeVorticityEdge'
      diagnostics % normalizedRelativeVorticityEdge % isVarArray = .false.
      diagnostics % normalizedRelativeVorticityEdge % isPersistent = .true.
      diagnostics % normalizedRelativeVorticityEdge % isActive = .true.
      allocate(diagnostics % normalizedRelativeVorticityEdge % array(nVertLevels, nEdges + 1))
      diagnostics % normalizedRelativeVorticityEdge % array = 0.0_RKIND
      diagnostics % normalizedRelativeVorticityEdge % dimSizes(1) = nVertLevels
      diagnostics % normalizedRelativeVorticityEdge % dimNames(1) = 'nVertLevels'
      diagnostics % normalizedRelativeVorticityEdge % dimSizes(2) = nEdges
      diagnostics % normalizedRelativeVorticityEdge % dimNames(2) = 'nEdges'
      diagnostics % normalizedRelativeVorticityEdge % hasTimeDimension = .true.
      nullify(diagnostics % normalizedRelativeVorticityEdge % prev)
      nullify(diagnostics % normalizedRelativeVorticityEdge % next)
      nullify(diagnostics % normalizedRelativeVorticityEdge % sendList)
      nullify(diagnostics % normalizedRelativeVorticityEdge % recvList)
      nullify(diagnostics % normalizedRelativeVorticityEdge % copyList)
      diagnostics % normalizedRelativeVorticityEdge % ioinfo % input = .false.
      diagnostics % normalizedRelativeVorticityEdge % ioinfo % sfc = .false.
      diagnostics % normalizedRelativeVorticityEdge % ioinfo % restart = .false.
      diagnostics % normalizedRelativeVorticityEdge % ioinfo % output = .false.
      diagnostics % normalizedRelativeVorticityEdge % block => b

      allocate(diagnostics % normalizedPlanetaryVorticityEdge)
      allocate(diagnostics % normalizedPlanetaryVorticityEdge % ioinfo)
      diagnostics % normalizedPlanetaryVorticityEdge % fieldName = 'normalizedPlanetaryVorticityEdge'
      diagnostics % normalizedPlanetaryVorticityEdge % isVarArray = .false.
      diagnostics % normalizedPlanetaryVorticityEdge % isPersistent = .true.
      diagnostics % normalizedPlanetaryVorticityEdge % isActive = .true.
      allocate(diagnostics % normalizedPlanetaryVorticityEdge % array(nVertLevels, nEdges + 1))
      diagnostics % normalizedPlanetaryVorticityEdge % array = 0.0_RKIND
      diagnostics % normalizedPlanetaryVorticityEdge % dimSizes(1) = nVertLevels
      diagnostics % normalizedPlanetaryVorticityEdge % dimNames(1) = 'nVertLevels'
      diagnostics % normalizedPlanetaryVorticityEdge % dimSizes(2) = nEdges
      diagnostics % normalizedPlanetaryVorticityEdge % dimNames(2) = 'nEdges'
      diagnostics % normalizedPlanetaryVorticityEdge % hasTimeDimension = .true.
      nullify(diagnostics % normalizedPlanetaryVorticityEdge % prev)
      nullify(diagnostics % normalizedPlanetaryVorticityEdge % next)
      nullify(diagnostics % normalizedPlanetaryVorticityEdge % sendList)
      nullify(diagnostics % normalizedPlanetaryVorticityEdge % recvList)
      nullify(diagnostics % normalizedPlanetaryVorticityEdge % copyList)
      diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % input = .false.
      diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % sfc = .false.
      diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % restart = .false.
      diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % output = .false.
      diagnostics % normalizedPlanetaryVorticityEdge % block => b

      allocate(diagnostics % normalizedRelativeVorticityCell)
      allocate(diagnostics % normalizedRelativeVorticityCell % ioinfo)
      diagnostics % normalizedRelativeVorticityCell % fieldName = 'normalizedRelativeVorticityCell'
      diagnostics % normalizedRelativeVorticityCell % isVarArray = .false.
      diagnostics % normalizedRelativeVorticityCell % isPersistent = .true.
      diagnostics % normalizedRelativeVorticityCell % isActive = .true.
      allocate(diagnostics % normalizedRelativeVorticityCell % array(nVertLevels, nCells + 1))
      diagnostics % normalizedRelativeVorticityCell % array = 0.0_RKIND
      diagnostics % normalizedRelativeVorticityCell % dimSizes(1) = nVertLevels
      diagnostics % normalizedRelativeVorticityCell % dimNames(1) = 'nVertLevels'
      diagnostics % normalizedRelativeVorticityCell % dimSizes(2) = nCells
      diagnostics % normalizedRelativeVorticityCell % dimNames(2) = 'nCells'
      diagnostics % normalizedRelativeVorticityCell % hasTimeDimension = .true.
      nullify(diagnostics % normalizedRelativeVorticityCell % prev)
      nullify(diagnostics % normalizedRelativeVorticityCell % next)
      nullify(diagnostics % normalizedRelativeVorticityCell % sendList)
      nullify(diagnostics % normalizedRelativeVorticityCell % recvList)
      nullify(diagnostics % normalizedRelativeVorticityCell % copyList)
      diagnostics % normalizedRelativeVorticityCell % ioinfo % input = .false.
      diagnostics % normalizedRelativeVorticityCell % ioinfo % sfc = .false.
      diagnostics % normalizedRelativeVorticityCell % ioinfo % restart = .false.
      diagnostics % normalizedRelativeVorticityCell % ioinfo % output = .false.
      diagnostics % normalizedRelativeVorticityCell % block => b

      allocate(diagnostics % barotropicForcing)
      allocate(diagnostics % barotropicForcing % ioinfo)
      diagnostics % barotropicForcing % fieldName = 'barotropicForcing'
      diagnostics % barotropicForcing % isVarArray = .false.
      diagnostics % barotropicForcing % isPersistent = .true.
      diagnostics % barotropicForcing % isActive = .true.
      allocate(diagnostics % barotropicForcing % array(nEdges + 1))
      diagnostics % barotropicForcing % array = 0.0_RKIND
      diagnostics % barotropicForcing % dimSizes(1) = nEdges
      diagnostics % barotropicForcing % dimNames(1) = 'nEdges'
      diagnostics % barotropicForcing % hasTimeDimension = .true.
      nullify(diagnostics % barotropicForcing % prev)
      nullify(diagnostics % barotropicForcing % next)
      nullify(diagnostics % barotropicForcing % sendList)
      nullify(diagnostics % barotropicForcing % recvList)
      nullify(diagnostics % barotropicForcing % copyList)
      diagnostics % barotropicForcing % ioinfo % input = .false.
      diagnostics % barotropicForcing % ioinfo % sfc = .false.
      diagnostics % barotropicForcing % ioinfo % restart = .false.
      diagnostics % barotropicForcing % ioinfo % output = .false.
      diagnostics % barotropicForcing % block => b

      allocate(diagnostics % barotropicThicknessFlux)
      allocate(diagnostics % barotropicThicknessFlux % ioinfo)
      diagnostics % barotropicThicknessFlux % fieldName = 'barotropicThicknessFlux'
      diagnostics % barotropicThicknessFlux % isVarArray = .false.
      diagnostics % barotropicThicknessFlux % isPersistent = .true.
      diagnostics % barotropicThicknessFlux % isActive = .true.
      allocate(diagnostics % barotropicThicknessFlux % array(nEdges + 1))
      diagnostics % barotropicThicknessFlux % array = 0.0_RKIND
      diagnostics % barotropicThicknessFlux % dimSizes(1) = nEdges
      diagnostics % barotropicThicknessFlux % dimNames(1) = 'nEdges'
      diagnostics % barotropicThicknessFlux % hasTimeDimension = .true.
      nullify(diagnostics % barotropicThicknessFlux % prev)
      nullify(diagnostics % barotropicThicknessFlux % next)
      nullify(diagnostics % barotropicThicknessFlux % sendList)
      nullify(diagnostics % barotropicThicknessFlux % recvList)
      nullify(diagnostics % barotropicThicknessFlux % copyList)
      diagnostics % barotropicThicknessFlux % ioinfo % input = .false.
      diagnostics % barotropicThicknessFlux % ioinfo % sfc = .false.
      diagnostics % barotropicThicknessFlux % ioinfo % restart = .false.
      diagnostics % barotropicThicknessFlux % ioinfo % output = .false.
      diagnostics % barotropicThicknessFlux % block => b

      allocate(diagnostics % normalVelocityX)
      allocate(diagnostics % normalVelocityX % ioinfo)
      diagnostics % normalVelocityX % fieldName = 'normalVelocityX'
      diagnostics % normalVelocityX % isVarArray = .false.
      diagnostics % normalVelocityX % isPersistent = .true.
      diagnostics % normalVelocityX % isActive = .true.
      allocate(diagnostics % normalVelocityX % array(nVertLevels, nCells + 1))
      diagnostics % normalVelocityX % array = 0.0_RKIND
      diagnostics % normalVelocityX % dimSizes(1) = nVertLevels
      diagnostics % normalVelocityX % dimNames(1) = 'nVertLevels'
      diagnostics % normalVelocityX % dimSizes(2) = nCells
      diagnostics % normalVelocityX % dimNames(2) = 'nCells'
      diagnostics % normalVelocityX % hasTimeDimension = .true.
      nullify(diagnostics % normalVelocityX % prev)
      nullify(diagnostics % normalVelocityX % next)
      nullify(diagnostics % normalVelocityX % sendList)
      nullify(diagnostics % normalVelocityX % recvList)
      nullify(diagnostics % normalVelocityX % copyList)
      diagnostics % normalVelocityX % ioinfo % input = .false.
      diagnostics % normalVelocityX % ioinfo % sfc = .false.
      diagnostics % normalVelocityX % ioinfo % restart = .false.
      diagnostics % normalVelocityX % ioinfo % output = .false.
      diagnostics % normalVelocityX % block => b

      allocate(diagnostics % normalVelocityY)
      allocate(diagnostics % normalVelocityY % ioinfo)
      diagnostics % normalVelocityY % fieldName = 'normalVelocityY'
      diagnostics % normalVelocityY % isVarArray = .false.
      diagnostics % normalVelocityY % isPersistent = .true.
      diagnostics % normalVelocityY % isActive = .true.
      allocate(diagnostics % normalVelocityY % array(nVertLevels, nCells + 1))
      diagnostics % normalVelocityY % array = 0.0_RKIND
      diagnostics % normalVelocityY % dimSizes(1) = nVertLevels
      diagnostics % normalVelocityY % dimNames(1) = 'nVertLevels'
      diagnostics % normalVelocityY % dimSizes(2) = nCells
      diagnostics % normalVelocityY % dimNames(2) = 'nCells'
      diagnostics % normalVelocityY % hasTimeDimension = .true.
      nullify(diagnostics % normalVelocityY % prev)
      nullify(diagnostics % normalVelocityY % next)
      nullify(diagnostics % normalVelocityY % sendList)
      nullify(diagnostics % normalVelocityY % recvList)
      nullify(diagnostics % normalVelocityY % copyList)
      diagnostics % normalVelocityY % ioinfo % input = .false.
      diagnostics % normalVelocityY % ioinfo % sfc = .false.
      diagnostics % normalVelocityY % ioinfo % restart = .false.
      diagnostics % normalVelocityY % ioinfo % output = .false.
      diagnostics % normalVelocityY % block => b

      allocate(diagnostics % normalVelocityZ)
      allocate(diagnostics % normalVelocityZ % ioinfo)
      diagnostics % normalVelocityZ % fieldName = 'normalVelocityZ'
      diagnostics % normalVelocityZ % isVarArray = .false.
      diagnostics % normalVelocityZ % isPersistent = .true.
      diagnostics % normalVelocityZ % isActive = .true.
      allocate(diagnostics % normalVelocityZ % array(nVertLevels, nCells + 1))
      diagnostics % normalVelocityZ % array = 0.0_RKIND
      diagnostics % normalVelocityZ % dimSizes(1) = nVertLevels
      diagnostics % normalVelocityZ % dimNames(1) = 'nVertLevels'
      diagnostics % normalVelocityZ % dimSizes(2) = nCells
      diagnostics % normalVelocityZ % dimNames(2) = 'nCells'
      diagnostics % normalVelocityZ % hasTimeDimension = .true.
      nullify(diagnostics % normalVelocityZ % prev)
      nullify(diagnostics % normalVelocityZ % next)
      nullify(diagnostics % normalVelocityZ % sendList)
      nullify(diagnostics % normalVelocityZ % recvList)
      nullify(diagnostics % normalVelocityZ % copyList)
      diagnostics % normalVelocityZ % ioinfo % input = .false.
      diagnostics % normalVelocityZ % ioinfo % sfc = .false.
      diagnostics % normalVelocityZ % ioinfo % restart = .false.
      diagnostics % normalVelocityZ % ioinfo % output = .false.
      diagnostics % normalVelocityZ % block => b

      allocate(diagnostics % normalVelocityZonal)
      allocate(diagnostics % normalVelocityZonal % ioinfo)
      diagnostics % normalVelocityZonal % fieldName = 'normalVelocityZonal'
      diagnostics % normalVelocityZonal % isVarArray = .false.
      diagnostics % normalVelocityZonal % isPersistent = .true.
      diagnostics % normalVelocityZonal % isActive = .true.
      allocate(diagnostics % normalVelocityZonal % array(nVertLevels, nCells + 1))
      diagnostics % normalVelocityZonal % array = 0.0_RKIND
      diagnostics % normalVelocityZonal % dimSizes(1) = nVertLevels
      diagnostics % normalVelocityZonal % dimNames(1) = 'nVertLevels'
      diagnostics % normalVelocityZonal % dimSizes(2) = nCells
      diagnostics % normalVelocityZonal % dimNames(2) = 'nCells'
      diagnostics % normalVelocityZonal % hasTimeDimension = .true.
      nullify(diagnostics % normalVelocityZonal % prev)
      nullify(diagnostics % normalVelocityZonal % next)
      nullify(diagnostics % normalVelocityZonal % sendList)
      nullify(diagnostics % normalVelocityZonal % recvList)
      nullify(diagnostics % normalVelocityZonal % copyList)
      diagnostics % normalVelocityZonal % ioinfo % input = .false.
      diagnostics % normalVelocityZonal % ioinfo % sfc = .false.
      diagnostics % normalVelocityZonal % ioinfo % restart = .false.
      diagnostics % normalVelocityZonal % ioinfo % output = .true.
      diagnostics % normalVelocityZonal % block => b

      allocate(diagnostics % normalVelocityMeridional)
      allocate(diagnostics % normalVelocityMeridional % ioinfo)
      diagnostics % normalVelocityMeridional % fieldName = 'normalVelocityMeridional'
      diagnostics % normalVelocityMeridional % isVarArray = .false.
      diagnostics % normalVelocityMeridional % isPersistent = .true.
      diagnostics % normalVelocityMeridional % isActive = .true.
      allocate(diagnostics % normalVelocityMeridional % array(nVertLevels, nCells + 1))
      diagnostics % normalVelocityMeridional % array = 0.0_RKIND
      diagnostics % normalVelocityMeridional % dimSizes(1) = nVertLevels
      diagnostics % normalVelocityMeridional % dimNames(1) = 'nVertLevels'
      diagnostics % normalVelocityMeridional % dimSizes(2) = nCells
      diagnostics % normalVelocityMeridional % dimNames(2) = 'nCells'
      diagnostics % normalVelocityMeridional % hasTimeDimension = .true.
      nullify(diagnostics % normalVelocityMeridional % prev)
      nullify(diagnostics % normalVelocityMeridional % next)
      nullify(diagnostics % normalVelocityMeridional % sendList)
      nullify(diagnostics % normalVelocityMeridional % recvList)
      nullify(diagnostics % normalVelocityMeridional % copyList)
      diagnostics % normalVelocityMeridional % ioinfo % input = .false.
      diagnostics % normalVelocityMeridional % ioinfo % sfc = .false.
      diagnostics % normalVelocityMeridional % ioinfo % restart = .false.
      diagnostics % normalVelocityMeridional % ioinfo % output = .true.
      diagnostics % normalVelocityMeridional % block => b

      allocate(diagnostics % gradSSH)
      allocate(diagnostics % gradSSH % ioinfo)
      diagnostics % gradSSH % fieldName = 'gradSSH'
      diagnostics % gradSSH % isVarArray = .false.
      diagnostics % gradSSH % isPersistent = .true.
      diagnostics % gradSSH % isActive = .true.
      allocate(diagnostics % gradSSH % array(nVertLevels, nEdges + 1))
      diagnostics % gradSSH % array = 0.0_RKIND
      diagnostics % gradSSH % dimSizes(1) = nVertLevels
      diagnostics % gradSSH % dimNames(1) = 'nVertLevels'
      diagnostics % gradSSH % dimSizes(2) = nEdges
      diagnostics % gradSSH % dimNames(2) = 'nEdges'
      diagnostics % gradSSH % hasTimeDimension = .true.
      nullify(diagnostics % gradSSH % prev)
      nullify(diagnostics % gradSSH % next)
      nullify(diagnostics % gradSSH % sendList)
      nullify(diagnostics % gradSSH % recvList)
      nullify(diagnostics % gradSSH % copyList)
      diagnostics % gradSSH % ioinfo % input = .false.
      diagnostics % gradSSH % ioinfo % sfc = .false.
      diagnostics % gradSSH % ioinfo % restart = .false.
      diagnostics % gradSSH % ioinfo % output = .false.
      diagnostics % gradSSH % block => b

      allocate(diagnostics % gradSSHX)
      allocate(diagnostics % gradSSHX % ioinfo)
      diagnostics % gradSSHX % fieldName = 'gradSSHX'
      diagnostics % gradSSHX % isVarArray = .false.
      diagnostics % gradSSHX % isPersistent = .true.
      diagnostics % gradSSHX % isActive = .true.
      allocate(diagnostics % gradSSHX % array(nVertLevels, nCells + 1))
      diagnostics % gradSSHX % array = 0.0_RKIND
      diagnostics % gradSSHX % dimSizes(1) = nVertLevels
      diagnostics % gradSSHX % dimNames(1) = 'nVertLevels'
      diagnostics % gradSSHX % dimSizes(2) = nCells
      diagnostics % gradSSHX % dimNames(2) = 'nCells'
      diagnostics % gradSSHX % hasTimeDimension = .true.
      nullify(diagnostics % gradSSHX % prev)
      nullify(diagnostics % gradSSHX % next)
      nullify(diagnostics % gradSSHX % sendList)
      nullify(diagnostics % gradSSHX % recvList)
      nullify(diagnostics % gradSSHX % copyList)
      diagnostics % gradSSHX % ioinfo % input = .false.
      diagnostics % gradSSHX % ioinfo % sfc = .false.
      diagnostics % gradSSHX % ioinfo % restart = .false.
      diagnostics % gradSSHX % ioinfo % output = .false.
      diagnostics % gradSSHX % block => b

      allocate(diagnostics % gradSSHY)
      allocate(diagnostics % gradSSHY % ioinfo)
      diagnostics % gradSSHY % fieldName = 'gradSSHY'
      diagnostics % gradSSHY % isVarArray = .false.
      diagnostics % gradSSHY % isPersistent = .true.
      diagnostics % gradSSHY % isActive = .true.
      allocate(diagnostics % gradSSHY % array(nVertLevels, nCells + 1))
      diagnostics % gradSSHY % array = 0.0_RKIND
      diagnostics % gradSSHY % dimSizes(1) = nVertLevels
      diagnostics % gradSSHY % dimNames(1) = 'nVertLevels'
      diagnostics % gradSSHY % dimSizes(2) = nCells
      diagnostics % gradSSHY % dimNames(2) = 'nCells'
      diagnostics % gradSSHY % hasTimeDimension = .true.
      nullify(diagnostics % gradSSHY % prev)
      nullify(diagnostics % gradSSHY % next)
      nullify(diagnostics % gradSSHY % sendList)
      nullify(diagnostics % gradSSHY % recvList)
      nullify(diagnostics % gradSSHY % copyList)
      diagnostics % gradSSHY % ioinfo % input = .false.
      diagnostics % gradSSHY % ioinfo % sfc = .false.
      diagnostics % gradSSHY % ioinfo % restart = .false.
      diagnostics % gradSSHY % ioinfo % output = .false.
      diagnostics % gradSSHY % block => b

      allocate(diagnostics % gradSSHZ)
      allocate(diagnostics % gradSSHZ % ioinfo)
      diagnostics % gradSSHZ % fieldName = 'gradSSHZ'
      diagnostics % gradSSHZ % isVarArray = .false.
      diagnostics % gradSSHZ % isPersistent = .true.
      diagnostics % gradSSHZ % isActive = .true.
      allocate(diagnostics % gradSSHZ % array(nVertLevels, nCells + 1))
      diagnostics % gradSSHZ % array = 0.0_RKIND
      diagnostics % gradSSHZ % dimSizes(1) = nVertLevels
      diagnostics % gradSSHZ % dimNames(1) = 'nVertLevels'
      diagnostics % gradSSHZ % dimSizes(2) = nCells
      diagnostics % gradSSHZ % dimNames(2) = 'nCells'
      diagnostics % gradSSHZ % hasTimeDimension = .true.
      nullify(diagnostics % gradSSHZ % prev)
      nullify(diagnostics % gradSSHZ % next)
      nullify(diagnostics % gradSSHZ % sendList)
      nullify(diagnostics % gradSSHZ % recvList)
      nullify(diagnostics % gradSSHZ % copyList)
      diagnostics % gradSSHZ % ioinfo % input = .false.
      diagnostics % gradSSHZ % ioinfo % sfc = .false.
      diagnostics % gradSSHZ % ioinfo % restart = .false.
      diagnostics % gradSSHZ % ioinfo % output = .false.
      diagnostics % gradSSHZ % block => b

      allocate(diagnostics % gradSSHZonal)
      allocate(diagnostics % gradSSHZonal % ioinfo)
      diagnostics % gradSSHZonal % fieldName = 'gradSSHZonal'
      diagnostics % gradSSHZonal % isVarArray = .false.
      diagnostics % gradSSHZonal % isPersistent = .true.
      diagnostics % gradSSHZonal % isActive = .true.
      allocate(diagnostics % gradSSHZonal % array(nVertLevels, nCells + 1))
      diagnostics % gradSSHZonal % array = 0.0_RKIND
      diagnostics % gradSSHZonal % dimSizes(1) = nVertLevels
      diagnostics % gradSSHZonal % dimNames(1) = 'nVertLevels'
      diagnostics % gradSSHZonal % dimSizes(2) = nCells
      diagnostics % gradSSHZonal % dimNames(2) = 'nCells'
      diagnostics % gradSSHZonal % hasTimeDimension = .true.
      nullify(diagnostics % gradSSHZonal % prev)
      nullify(diagnostics % gradSSHZonal % next)
      nullify(diagnostics % gradSSHZonal % sendList)
      nullify(diagnostics % gradSSHZonal % recvList)
      nullify(diagnostics % gradSSHZonal % copyList)
      diagnostics % gradSSHZonal % ioinfo % input = .false.
      diagnostics % gradSSHZonal % ioinfo % sfc = .false.
      diagnostics % gradSSHZonal % ioinfo % restart = .false.
      diagnostics % gradSSHZonal % ioinfo % output = .false.
      diagnostics % gradSSHZonal % block => b

      allocate(diagnostics % gradSSHMeridional)
      allocate(diagnostics % gradSSHMeridional % ioinfo)
      diagnostics % gradSSHMeridional % fieldName = 'gradSSHMeridional'
      diagnostics % gradSSHMeridional % isVarArray = .false.
      diagnostics % gradSSHMeridional % isPersistent = .true.
      diagnostics % gradSSHMeridional % isActive = .true.
      allocate(diagnostics % gradSSHMeridional % array(nVertLevels, nCells + 1))
      diagnostics % gradSSHMeridional % array = 0.0_RKIND
      diagnostics % gradSSHMeridional % dimSizes(1) = nVertLevels
      diagnostics % gradSSHMeridional % dimNames(1) = 'nVertLevels'
      diagnostics % gradSSHMeridional % dimSizes(2) = nCells
      diagnostics % gradSSHMeridional % dimNames(2) = 'nCells'
      diagnostics % gradSSHMeridional % hasTimeDimension = .true.
      nullify(diagnostics % gradSSHMeridional % prev)
      nullify(diagnostics % gradSSHMeridional % next)
      nullify(diagnostics % gradSSHMeridional % sendList)
      nullify(diagnostics % gradSSHMeridional % recvList)
      nullify(diagnostics % gradSSHMeridional % copyList)
      diagnostics % gradSSHMeridional % ioinfo % input = .false.
      diagnostics % gradSSHMeridional % ioinfo % sfc = .false.
      diagnostics % gradSSHMeridional % ioinfo % restart = .false.
      diagnostics % gradSSHMeridional % ioinfo % output = .false.
      diagnostics % gradSSHMeridional % block => b

      allocate(diagnostics % uBolusGM)
      allocate(diagnostics % uBolusGM % ioinfo)
      diagnostics % uBolusGM % fieldName = 'uBolusGM'
      diagnostics % uBolusGM % isVarArray = .false.
      diagnostics % uBolusGM % isPersistent = .true.
      diagnostics % uBolusGM % isActive = .true.
      allocate(diagnostics % uBolusGM % array(nVertLevels, nEdges + 1))
      diagnostics % uBolusGM % array = 0.0_RKIND
      diagnostics % uBolusGM % dimSizes(1) = nVertLevels
      diagnostics % uBolusGM % dimNames(1) = 'nVertLevels'
      diagnostics % uBolusGM % dimSizes(2) = nEdges
      diagnostics % uBolusGM % dimNames(2) = 'nEdges'
      diagnostics % uBolusGM % hasTimeDimension = .true.
      nullify(diagnostics % uBolusGM % prev)
      nullify(diagnostics % uBolusGM % next)
      nullify(diagnostics % uBolusGM % sendList)
      nullify(diagnostics % uBolusGM % recvList)
      nullify(diagnostics % uBolusGM % copyList)
      diagnostics % uBolusGM % ioinfo % input = .false.
      diagnostics % uBolusGM % ioinfo % sfc = .false.
      diagnostics % uBolusGM % ioinfo % restart = .false.
      diagnostics % uBolusGM % ioinfo % output = .false.
      diagnostics % uBolusGM % block => b

      allocate(diagnostics % uBolusGMX)
      allocate(diagnostics % uBolusGMX % ioinfo)
      diagnostics % uBolusGMX % fieldName = 'uBolusGMX'
      diagnostics % uBolusGMX % isVarArray = .false.
      diagnostics % uBolusGMX % isPersistent = .true.
      diagnostics % uBolusGMX % isActive = .true.
      allocate(diagnostics % uBolusGMX % array(nVertLevels, nEdges + 1))
      diagnostics % uBolusGMX % array = 0.0_RKIND
      diagnostics % uBolusGMX % dimSizes(1) = nVertLevels
      diagnostics % uBolusGMX % dimNames(1) = 'nVertLevels'
      diagnostics % uBolusGMX % dimSizes(2) = nEdges
      diagnostics % uBolusGMX % dimNames(2) = 'nEdges'
      diagnostics % uBolusGMX % hasTimeDimension = .true.
      nullify(diagnostics % uBolusGMX % prev)
      nullify(diagnostics % uBolusGMX % next)
      nullify(diagnostics % uBolusGMX % sendList)
      nullify(diagnostics % uBolusGMX % recvList)
      nullify(diagnostics % uBolusGMX % copyList)
      diagnostics % uBolusGMX % ioinfo % input = .false.
      diagnostics % uBolusGMX % ioinfo % sfc = .false.
      diagnostics % uBolusGMX % ioinfo % restart = .false.
      diagnostics % uBolusGMX % ioinfo % output = .false.
      diagnostics % uBolusGMX % block => b

      allocate(diagnostics % uBolusGMY)
      allocate(diagnostics % uBolusGMY % ioinfo)
      diagnostics % uBolusGMY % fieldName = 'uBolusGMY'
      diagnostics % uBolusGMY % isVarArray = .false.
      diagnostics % uBolusGMY % isPersistent = .true.
      diagnostics % uBolusGMY % isActive = .true.
      allocate(diagnostics % uBolusGMY % array(nVertLevels, nEdges + 1))
      diagnostics % uBolusGMY % array = 0.0_RKIND
      diagnostics % uBolusGMY % dimSizes(1) = nVertLevels
      diagnostics % uBolusGMY % dimNames(1) = 'nVertLevels'
      diagnostics % uBolusGMY % dimSizes(2) = nEdges
      diagnostics % uBolusGMY % dimNames(2) = 'nEdges'
      diagnostics % uBolusGMY % hasTimeDimension = .true.
      nullify(diagnostics % uBolusGMY % prev)
      nullify(diagnostics % uBolusGMY % next)
      nullify(diagnostics % uBolusGMY % sendList)
      nullify(diagnostics % uBolusGMY % recvList)
      nullify(diagnostics % uBolusGMY % copyList)
      diagnostics % uBolusGMY % ioinfo % input = .false.
      diagnostics % uBolusGMY % ioinfo % sfc = .false.
      diagnostics % uBolusGMY % ioinfo % restart = .false.
      diagnostics % uBolusGMY % ioinfo % output = .false.
      diagnostics % uBolusGMY % block => b

      allocate(diagnostics % uBolusGMZ)
      allocate(diagnostics % uBolusGMZ % ioinfo)
      diagnostics % uBolusGMZ % fieldName = 'uBolusGMZ'
      diagnostics % uBolusGMZ % isVarArray = .false.
      diagnostics % uBolusGMZ % isPersistent = .true.
      diagnostics % uBolusGMZ % isActive = .true.
      allocate(diagnostics % uBolusGMZ % array(nVertLevels, nEdges + 1))
      diagnostics % uBolusGMZ % array = 0.0_RKIND
      diagnostics % uBolusGMZ % dimSizes(1) = nVertLevels
      diagnostics % uBolusGMZ % dimNames(1) = 'nVertLevels'
      diagnostics % uBolusGMZ % dimSizes(2) = nEdges
      diagnostics % uBolusGMZ % dimNames(2) = 'nEdges'
      diagnostics % uBolusGMZ % hasTimeDimension = .true.
      nullify(diagnostics % uBolusGMZ % prev)
      nullify(diagnostics % uBolusGMZ % next)
      nullify(diagnostics % uBolusGMZ % sendList)
      nullify(diagnostics % uBolusGMZ % recvList)
      nullify(diagnostics % uBolusGMZ % copyList)
      diagnostics % uBolusGMZ % ioinfo % input = .false.
      diagnostics % uBolusGMZ % ioinfo % sfc = .false.
      diagnostics % uBolusGMZ % ioinfo % restart = .false.
      diagnostics % uBolusGMZ % ioinfo % output = .false.
      diagnostics % uBolusGMZ % block => b

      allocate(diagnostics % uBolusGMZonal)
      allocate(diagnostics % uBolusGMZonal % ioinfo)
      diagnostics % uBolusGMZonal % fieldName = 'uBolusGMZonal'
      diagnostics % uBolusGMZonal % isVarArray = .false.
      diagnostics % uBolusGMZonal % isPersistent = .true.
      diagnostics % uBolusGMZonal % isActive = .true.
      allocate(diagnostics % uBolusGMZonal % array(nVertLevels, nEdges + 1))
      diagnostics % uBolusGMZonal % array = 0.0_RKIND
      diagnostics % uBolusGMZonal % dimSizes(1) = nVertLevels
      diagnostics % uBolusGMZonal % dimNames(1) = 'nVertLevels'
      diagnostics % uBolusGMZonal % dimSizes(2) = nEdges
      diagnostics % uBolusGMZonal % dimNames(2) = 'nEdges'
      diagnostics % uBolusGMZonal % hasTimeDimension = .true.
      nullify(diagnostics % uBolusGMZonal % prev)
      nullify(diagnostics % uBolusGMZonal % next)
      nullify(diagnostics % uBolusGMZonal % sendList)
      nullify(diagnostics % uBolusGMZonal % recvList)
      nullify(diagnostics % uBolusGMZonal % copyList)
      diagnostics % uBolusGMZonal % ioinfo % input = .false.
      diagnostics % uBolusGMZonal % ioinfo % sfc = .false.
      diagnostics % uBolusGMZonal % ioinfo % restart = .false.
      diagnostics % uBolusGMZonal % ioinfo % output = .false.
      diagnostics % uBolusGMZonal % block => b

      allocate(diagnostics % uBolusGMMeridional)
      allocate(diagnostics % uBolusGMMeridional % ioinfo)
      diagnostics % uBolusGMMeridional % fieldName = 'uBolusGMMeridional'
      diagnostics % uBolusGMMeridional % isVarArray = .false.
      diagnostics % uBolusGMMeridional % isPersistent = .true.
      diagnostics % uBolusGMMeridional % isActive = .true.
      allocate(diagnostics % uBolusGMMeridional % array(nVertLevels, nEdges + 1))
      diagnostics % uBolusGMMeridional % array = 0.0_RKIND
      diagnostics % uBolusGMMeridional % dimSizes(1) = nVertLevels
      diagnostics % uBolusGMMeridional % dimNames(1) = 'nVertLevels'
      diagnostics % uBolusGMMeridional % dimSizes(2) = nEdges
      diagnostics % uBolusGMMeridional % dimNames(2) = 'nEdges'
      diagnostics % uBolusGMMeridional % hasTimeDimension = .true.
      nullify(diagnostics % uBolusGMMeridional % prev)
      nullify(diagnostics % uBolusGMMeridional % next)
      nullify(diagnostics % uBolusGMMeridional % sendList)
      nullify(diagnostics % uBolusGMMeridional % recvList)
      nullify(diagnostics % uBolusGMMeridional % copyList)
      diagnostics % uBolusGMMeridional % ioinfo % input = .false.
      diagnostics % uBolusGMMeridional % ioinfo % sfc = .false.
      diagnostics % uBolusGMMeridional % ioinfo % restart = .false.
      diagnostics % uBolusGMMeridional % ioinfo % output = .false.
      diagnostics % uBolusGMMeridional % block => b

      allocate(diagnostics % RiTopOfCell)
      allocate(diagnostics % RiTopOfCell % ioinfo)
      diagnostics % RiTopOfCell % fieldName = 'RiTopOfCell'
      diagnostics % RiTopOfCell % isVarArray = .false.
      diagnostics % RiTopOfCell % isPersistent = .true.
      diagnostics % RiTopOfCell % isActive = .true.
      allocate(diagnostics % RiTopOfCell % array(nVertLevels+1, nCells + 1))
      diagnostics % RiTopOfCell % array = 0.0_RKIND
      diagnostics % RiTopOfCell % dimSizes(1) = nVertLevels+1
      diagnostics % RiTopOfCell % dimNames(1) = 'nVertLevelsP1'
      diagnostics % RiTopOfCell % dimSizes(2) = nCells
      diagnostics % RiTopOfCell % dimNames(2) = 'nCells'
      diagnostics % RiTopOfCell % hasTimeDimension = .true.
      nullify(diagnostics % RiTopOfCell % prev)
      nullify(diagnostics % RiTopOfCell % next)
      nullify(diagnostics % RiTopOfCell % sendList)
      nullify(diagnostics % RiTopOfCell % recvList)
      nullify(diagnostics % RiTopOfCell % copyList)
      diagnostics % RiTopOfCell % ioinfo % input = .false.
      diagnostics % RiTopOfCell % ioinfo % sfc = .false.
      diagnostics % RiTopOfCell % ioinfo % restart = .false.
      diagnostics % RiTopOfCell % ioinfo % output = .true.
      diagnostics % RiTopOfCell % block => b

      allocate(diagnostics % RiTopOfEdge)
      allocate(diagnostics % RiTopOfEdge % ioinfo)
      diagnostics % RiTopOfEdge % fieldName = 'RiTopOfEdge'
      diagnostics % RiTopOfEdge % isVarArray = .false.
      diagnostics % RiTopOfEdge % isPersistent = .true.
      diagnostics % RiTopOfEdge % isActive = .true.
      allocate(diagnostics % RiTopOfEdge % array(nVertLevels+1, nEdges + 1))
      diagnostics % RiTopOfEdge % array = 0.0_RKIND
      diagnostics % RiTopOfEdge % dimSizes(1) = nVertLevels+1
      diagnostics % RiTopOfEdge % dimNames(1) = 'nVertLevelsP1'
      diagnostics % RiTopOfEdge % dimSizes(2) = nEdges
      diagnostics % RiTopOfEdge % dimNames(2) = 'nEdges'
      diagnostics % RiTopOfEdge % hasTimeDimension = .true.
      nullify(diagnostics % RiTopOfEdge % prev)
      nullify(diagnostics % RiTopOfEdge % next)
      nullify(diagnostics % RiTopOfEdge % sendList)
      nullify(diagnostics % RiTopOfEdge % recvList)
      nullify(diagnostics % RiTopOfEdge % copyList)
      diagnostics % RiTopOfEdge % ioinfo % input = .false.
      diagnostics % RiTopOfEdge % ioinfo % sfc = .false.
      diagnostics % RiTopOfEdge % ioinfo % restart = .false.
      diagnostics % RiTopOfEdge % ioinfo % output = .false.
      diagnostics % RiTopOfEdge % block => b

      allocate(diagnostics % vertViscTopOfEdge)
      allocate(diagnostics % vertViscTopOfEdge % ioinfo)
      diagnostics % vertViscTopOfEdge % fieldName = 'vertViscTopOfEdge'
      diagnostics % vertViscTopOfEdge % isVarArray = .false.
      diagnostics % vertViscTopOfEdge % isPersistent = .true.
      diagnostics % vertViscTopOfEdge % isActive = .true.
      allocate(diagnostics % vertViscTopOfEdge % array(nVertLevels+1, nEdges + 1))
      diagnostics % vertViscTopOfEdge % array = 0.0_RKIND
      diagnostics % vertViscTopOfEdge % dimSizes(1) = nVertLevels+1
      diagnostics % vertViscTopOfEdge % dimNames(1) = 'nVertLevelsP1'
      diagnostics % vertViscTopOfEdge % dimSizes(2) = nEdges
      diagnostics % vertViscTopOfEdge % dimNames(2) = 'nEdges'
      diagnostics % vertViscTopOfEdge % hasTimeDimension = .true.
      nullify(diagnostics % vertViscTopOfEdge % prev)
      nullify(diagnostics % vertViscTopOfEdge % next)
      nullify(diagnostics % vertViscTopOfEdge % sendList)
      nullify(diagnostics % vertViscTopOfEdge % recvList)
      nullify(diagnostics % vertViscTopOfEdge % copyList)
      diagnostics % vertViscTopOfEdge % ioinfo % input = .false.
      diagnostics % vertViscTopOfEdge % ioinfo % sfc = .false.
      diagnostics % vertViscTopOfEdge % ioinfo % restart = .false.
      diagnostics % vertViscTopOfEdge % ioinfo % output = .true.
      diagnostics % vertViscTopOfEdge % block => b

      allocate(diagnostics % vertViscTopOfCell)
      allocate(diagnostics % vertViscTopOfCell % ioinfo)
      diagnostics % vertViscTopOfCell % fieldName = 'vertViscTopOfCell'
      diagnostics % vertViscTopOfCell % isVarArray = .false.
      diagnostics % vertViscTopOfCell % isPersistent = .true.
      diagnostics % vertViscTopOfCell % isActive = .true.
      allocate(diagnostics % vertViscTopOfCell % array(nVertLevels+1, nCells + 1))
      diagnostics % vertViscTopOfCell % array = 0.0_RKIND
      diagnostics % vertViscTopOfCell % dimSizes(1) = nVertLevels+1
      diagnostics % vertViscTopOfCell % dimNames(1) = 'nVertLevelsP1'
      diagnostics % vertViscTopOfCell % dimSizes(2) = nCells
      diagnostics % vertViscTopOfCell % dimNames(2) = 'nCells'
      diagnostics % vertViscTopOfCell % hasTimeDimension = .true.
      nullify(diagnostics % vertViscTopOfCell % prev)
      nullify(diagnostics % vertViscTopOfCell % next)
      nullify(diagnostics % vertViscTopOfCell % sendList)
      nullify(diagnostics % vertViscTopOfCell % recvList)
      nullify(diagnostics % vertViscTopOfCell % copyList)
      diagnostics % vertViscTopOfCell % ioinfo % input = .false.
      diagnostics % vertViscTopOfCell % ioinfo % sfc = .false.
      diagnostics % vertViscTopOfCell % ioinfo % restart = .false.
      diagnostics % vertViscTopOfCell % ioinfo % output = .true.
      diagnostics % vertViscTopOfCell % block => b

      allocate(diagnostics % vertDiffTopOfCell)
      allocate(diagnostics % vertDiffTopOfCell % ioinfo)
      diagnostics % vertDiffTopOfCell % fieldName = 'vertDiffTopOfCell'
      diagnostics % vertDiffTopOfCell % isVarArray = .false.
      diagnostics % vertDiffTopOfCell % isPersistent = .true.
      diagnostics % vertDiffTopOfCell % isActive = .true.
      allocate(diagnostics % vertDiffTopOfCell % array(nVertLevels+1, nCells + 1))
      diagnostics % vertDiffTopOfCell % array = 0.0_RKIND
      diagnostics % vertDiffTopOfCell % dimSizes(1) = nVertLevels+1
      diagnostics % vertDiffTopOfCell % dimNames(1) = 'nVertLevelsP1'
      diagnostics % vertDiffTopOfCell % dimSizes(2) = nCells
      diagnostics % vertDiffTopOfCell % dimNames(2) = 'nCells'
      diagnostics % vertDiffTopOfCell % hasTimeDimension = .true.
      nullify(diagnostics % vertDiffTopOfCell % prev)
      nullify(diagnostics % vertDiffTopOfCell % next)
      nullify(diagnostics % vertDiffTopOfCell % sendList)
      nullify(diagnostics % vertDiffTopOfCell % recvList)
      nullify(diagnostics % vertDiffTopOfCell % copyList)
      diagnostics % vertDiffTopOfCell % ioinfo % input = .false.
      diagnostics % vertDiffTopOfCell % ioinfo % sfc = .false.
      diagnostics % vertDiffTopOfCell % ioinfo % restart = .false.
      diagnostics % vertDiffTopOfCell % ioinfo % output = .true.
      diagnostics % vertDiffTopOfCell % block => b

      allocate(diagnostics % bulkRichardsonNumber)
      allocate(diagnostics % bulkRichardsonNumber % ioinfo)
      diagnostics % bulkRichardsonNumber % fieldName = 'bulkRichardsonNumber'
      diagnostics % bulkRichardsonNumber % isVarArray = .false.
      diagnostics % bulkRichardsonNumber % isPersistent = .true.
      diagnostics % bulkRichardsonNumber % isActive = .true.
      allocate(diagnostics % bulkRichardsonNumber % array(nVertLevels, nCells + 1))
      diagnostics % bulkRichardsonNumber % array = 0.0_RKIND
      diagnostics % bulkRichardsonNumber % dimSizes(1) = nVertLevels
      diagnostics % bulkRichardsonNumber % dimNames(1) = 'nVertLevels'
      diagnostics % bulkRichardsonNumber % dimSizes(2) = nCells
      diagnostics % bulkRichardsonNumber % dimNames(2) = 'nCells'
      diagnostics % bulkRichardsonNumber % hasTimeDimension = .true.
      nullify(diagnostics % bulkRichardsonNumber % prev)
      nullify(diagnostics % bulkRichardsonNumber % next)
      nullify(diagnostics % bulkRichardsonNumber % sendList)
      nullify(diagnostics % bulkRichardsonNumber % recvList)
      nullify(diagnostics % bulkRichardsonNumber % copyList)
      diagnostics % bulkRichardsonNumber % ioinfo % input = .false.
      diagnostics % bulkRichardsonNumber % ioinfo % sfc = .false.
      diagnostics % bulkRichardsonNumber % ioinfo % restart = .false.
      diagnostics % bulkRichardsonNumber % ioinfo % output = .true.
      diagnostics % bulkRichardsonNumber % block => b

      allocate(diagnostics % boundaryLayerDepth)
      allocate(diagnostics % boundaryLayerDepth % ioinfo)
      diagnostics % boundaryLayerDepth % fieldName = 'boundaryLayerDepth'
      diagnostics % boundaryLayerDepth % isVarArray = .false.
      diagnostics % boundaryLayerDepth % isPersistent = .true.
      diagnostics % boundaryLayerDepth % isActive = .true.
      allocate(diagnostics % boundaryLayerDepth % array(nCells + 1))
      diagnostics % boundaryLayerDepth % array = 0.0_RKIND
      diagnostics % boundaryLayerDepth % dimSizes(1) = nCells
      diagnostics % boundaryLayerDepth % dimNames(1) = 'nCells'
      diagnostics % boundaryLayerDepth % hasTimeDimension = .true.
      nullify(diagnostics % boundaryLayerDepth % prev)
      nullify(diagnostics % boundaryLayerDepth % next)
      nullify(diagnostics % boundaryLayerDepth % sendList)
      nullify(diagnostics % boundaryLayerDepth % recvList)
      nullify(diagnostics % boundaryLayerDepth % copyList)
      diagnostics % boundaryLayerDepth % ioinfo % input = .false.
      diagnostics % boundaryLayerDepth % ioinfo % sfc = .false.
      diagnostics % boundaryLayerDepth % ioinfo % restart = .false.
      diagnostics % boundaryLayerDepth % ioinfo % output = .true.
      diagnostics % boundaryLayerDepth % block => b

      allocate(diagnostics % indexBoundaryLayerDepth)
      allocate(diagnostics % indexBoundaryLayerDepth % ioinfo)
      diagnostics % indexBoundaryLayerDepth % fieldName = 'indexBoundaryLayerDepth'
      diagnostics % indexBoundaryLayerDepth % isVarArray = .false.
      diagnostics % indexBoundaryLayerDepth % isPersistent = .true.
      diagnostics % indexBoundaryLayerDepth % isActive = .true.
      allocate(diagnostics % indexBoundaryLayerDepth % array(nCells + 1))
      diagnostics % indexBoundaryLayerDepth % array = 0.0_RKIND
      diagnostics % indexBoundaryLayerDepth % dimSizes(1) = nCells
      diagnostics % indexBoundaryLayerDepth % dimNames(1) = 'nCells'
      diagnostics % indexBoundaryLayerDepth % hasTimeDimension = .true.
      nullify(diagnostics % indexBoundaryLayerDepth % prev)
      nullify(diagnostics % indexBoundaryLayerDepth % next)
      nullify(diagnostics % indexBoundaryLayerDepth % sendList)
      nullify(diagnostics % indexBoundaryLayerDepth % recvList)
      nullify(diagnostics % indexBoundaryLayerDepth % copyList)
      diagnostics % indexBoundaryLayerDepth % ioinfo % input = .false.
      diagnostics % indexBoundaryLayerDepth % ioinfo % sfc = .false.
      diagnostics % indexBoundaryLayerDepth % ioinfo % restart = .false.
      diagnostics % indexBoundaryLayerDepth % ioinfo % output = .true.
      diagnostics % indexBoundaryLayerDepth % block => b

      allocate(diagnostics % surfaceFrictionVelocity)
      allocate(diagnostics % surfaceFrictionVelocity % ioinfo)
      diagnostics % surfaceFrictionVelocity % fieldName = 'surfaceFrictionVelocity'
      diagnostics % surfaceFrictionVelocity % isVarArray = .false.
      diagnostics % surfaceFrictionVelocity % isPersistent = .true.
      diagnostics % surfaceFrictionVelocity % isActive = .true.
      allocate(diagnostics % surfaceFrictionVelocity % array(nCells + 1))
      diagnostics % surfaceFrictionVelocity % array = 0.0_RKIND
      diagnostics % surfaceFrictionVelocity % dimSizes(1) = nCells
      diagnostics % surfaceFrictionVelocity % dimNames(1) = 'nCells'
      diagnostics % surfaceFrictionVelocity % hasTimeDimension = .true.
      nullify(diagnostics % surfaceFrictionVelocity % prev)
      nullify(diagnostics % surfaceFrictionVelocity % next)
      nullify(diagnostics % surfaceFrictionVelocity % sendList)
      nullify(diagnostics % surfaceFrictionVelocity % recvList)
      nullify(diagnostics % surfaceFrictionVelocity % copyList)
      diagnostics % surfaceFrictionVelocity % ioinfo % input = .false.
      diagnostics % surfaceFrictionVelocity % ioinfo % sfc = .false.
      diagnostics % surfaceFrictionVelocity % ioinfo % restart = .false.
      diagnostics % surfaceFrictionVelocity % ioinfo % output = .true.
      diagnostics % surfaceFrictionVelocity % block => b

      allocate(diagnostics % windStressZonalDiag)
      allocate(diagnostics % windStressZonalDiag % ioinfo)
      diagnostics % windStressZonalDiag % fieldName = 'windStressZonalDiag'
      diagnostics % windStressZonalDiag % isVarArray = .false.
      diagnostics % windStressZonalDiag % isPersistent = .true.
      diagnostics % windStressZonalDiag % isActive = .true.
      allocate(diagnostics % windStressZonalDiag % array(nCells + 1))
      diagnostics % windStressZonalDiag % array = 0.0_RKIND
      diagnostics % windStressZonalDiag % dimSizes(1) = nCells
      diagnostics % windStressZonalDiag % dimNames(1) = 'nCells'
      diagnostics % windStressZonalDiag % hasTimeDimension = .true.
      nullify(diagnostics % windStressZonalDiag % prev)
      nullify(diagnostics % windStressZonalDiag % next)
      nullify(diagnostics % windStressZonalDiag % sendList)
      nullify(diagnostics % windStressZonalDiag % recvList)
      nullify(diagnostics % windStressZonalDiag % copyList)
      diagnostics % windStressZonalDiag % ioinfo % input = .false.
      diagnostics % windStressZonalDiag % ioinfo % sfc = .false.
      diagnostics % windStressZonalDiag % ioinfo % restart = .false.
      diagnostics % windStressZonalDiag % ioinfo % output = .true.
      diagnostics % windStressZonalDiag % block => b

      allocate(diagnostics % windStressMeridionalDiag)
      allocate(diagnostics % windStressMeridionalDiag % ioinfo)
      diagnostics % windStressMeridionalDiag % fieldName = 'windStressMeridionalDiag'
      diagnostics % windStressMeridionalDiag % isVarArray = .false.
      diagnostics % windStressMeridionalDiag % isPersistent = .true.
      diagnostics % windStressMeridionalDiag % isActive = .true.
      allocate(diagnostics % windStressMeridionalDiag % array(nCells + 1))
      diagnostics % windStressMeridionalDiag % array = 0.0_RKIND
      diagnostics % windStressMeridionalDiag % dimSizes(1) = nCells
      diagnostics % windStressMeridionalDiag % dimNames(1) = 'nCells'
      diagnostics % windStressMeridionalDiag % hasTimeDimension = .true.
      nullify(diagnostics % windStressMeridionalDiag % prev)
      nullify(diagnostics % windStressMeridionalDiag % next)
      nullify(diagnostics % windStressMeridionalDiag % sendList)
      nullify(diagnostics % windStressMeridionalDiag % recvList)
      nullify(diagnostics % windStressMeridionalDiag % copyList)
      diagnostics % windStressMeridionalDiag % ioinfo % input = .false.
      diagnostics % windStressMeridionalDiag % ioinfo % sfc = .false.
      diagnostics % windStressMeridionalDiag % ioinfo % restart = .false.
      diagnostics % windStressMeridionalDiag % ioinfo % output = .true.
      diagnostics % windStressMeridionalDiag % block => b

      allocate(diagnostics % penetrativeTemperatureFluxOBL)
      allocate(diagnostics % penetrativeTemperatureFluxOBL % ioinfo)
      diagnostics % penetrativeTemperatureFluxOBL % fieldName = 'penetrativeTemperatureFluxOBL'
      diagnostics % penetrativeTemperatureFluxOBL % isVarArray = .false.
      diagnostics % penetrativeTemperatureFluxOBL % isPersistent = .true.
      diagnostics % penetrativeTemperatureFluxOBL % isActive = .true.
      allocate(diagnostics % penetrativeTemperatureFluxOBL % array(nCells + 1))
      diagnostics % penetrativeTemperatureFluxOBL % array = 0.0_RKIND
      diagnostics % penetrativeTemperatureFluxOBL % dimSizes(1) = nCells
      diagnostics % penetrativeTemperatureFluxOBL % dimNames(1) = 'nCells'
      diagnostics % penetrativeTemperatureFluxOBL % hasTimeDimension = .true.
      nullify(diagnostics % penetrativeTemperatureFluxOBL % prev)
      nullify(diagnostics % penetrativeTemperatureFluxOBL % next)
      nullify(diagnostics % penetrativeTemperatureFluxOBL % sendList)
      nullify(diagnostics % penetrativeTemperatureFluxOBL % recvList)
      nullify(diagnostics % penetrativeTemperatureFluxOBL % copyList)
      diagnostics % penetrativeTemperatureFluxOBL % ioinfo % input = .false.
      diagnostics % penetrativeTemperatureFluxOBL % ioinfo % sfc = .false.
      diagnostics % penetrativeTemperatureFluxOBL % ioinfo % restart = .false.
      diagnostics % penetrativeTemperatureFluxOBL % ioinfo % output = .false.
      diagnostics % penetrativeTemperatureFluxOBL % block => b

      allocate(diagnostics % buoyancyForcingOBL)
      allocate(diagnostics % buoyancyForcingOBL % ioinfo)
      diagnostics % buoyancyForcingOBL % fieldName = 'buoyancyForcingOBL'
      diagnostics % buoyancyForcingOBL % isVarArray = .false.
      diagnostics % buoyancyForcingOBL % isPersistent = .true.
      diagnostics % buoyancyForcingOBL % isActive = .true.
      allocate(diagnostics % buoyancyForcingOBL % array(nCells + 1))
      diagnostics % buoyancyForcingOBL % array = 0.0_RKIND
      diagnostics % buoyancyForcingOBL % dimSizes(1) = nCells
      diagnostics % buoyancyForcingOBL % dimNames(1) = 'nCells'
      diagnostics % buoyancyForcingOBL % hasTimeDimension = .true.
      nullify(diagnostics % buoyancyForcingOBL % prev)
      nullify(diagnostics % buoyancyForcingOBL % next)
      nullify(diagnostics % buoyancyForcingOBL % sendList)
      nullify(diagnostics % buoyancyForcingOBL % recvList)
      nullify(diagnostics % buoyancyForcingOBL % copyList)
      diagnostics % buoyancyForcingOBL % ioinfo % input = .false.
      diagnostics % buoyancyForcingOBL % ioinfo % sfc = .false.
      diagnostics % buoyancyForcingOBL % ioinfo % restart = .false.
      diagnostics % buoyancyForcingOBL % ioinfo % output = .true.
      diagnostics % buoyancyForcingOBL % block => b

      allocate(diagnostics % areaCellGlobal)
      allocate(diagnostics % areaCellGlobal % ioinfo)
      diagnostics % areaCellGlobal % fieldName = 'areaCellGlobal'
      diagnostics % areaCellGlobal % isVarArray = .false.
      diagnostics % areaCellGlobal % hasTimeDimension = .true.
      nullify(diagnostics % areaCellGlobal % prev)
      nullify(diagnostics % areaCellGlobal % next)
      nullify(diagnostics % areaCellGlobal % sendList)
      nullify(diagnostics % areaCellGlobal % recvList)
      nullify(diagnostics % areaCellGlobal % copyList)
      diagnostics % areaCellGlobal % ioinfo % input = .false.
      diagnostics % areaCellGlobal % ioinfo % sfc = .false.
      diagnostics % areaCellGlobal % ioinfo % restart = .false.
      diagnostics % areaCellGlobal % ioinfo % output = .true.
      diagnostics % areaCellGlobal % block => b

      allocate(diagnostics % areaEdgeGlobal)
      allocate(diagnostics % areaEdgeGlobal % ioinfo)
      diagnostics % areaEdgeGlobal % fieldName = 'areaEdgeGlobal'
      diagnostics % areaEdgeGlobal % isVarArray = .false.
      diagnostics % areaEdgeGlobal % hasTimeDimension = .true.
      nullify(diagnostics % areaEdgeGlobal % prev)
      nullify(diagnostics % areaEdgeGlobal % next)
      nullify(diagnostics % areaEdgeGlobal % sendList)
      nullify(diagnostics % areaEdgeGlobal % recvList)
      nullify(diagnostics % areaEdgeGlobal % copyList)
      diagnostics % areaEdgeGlobal % ioinfo % input = .false.
      diagnostics % areaEdgeGlobal % ioinfo % sfc = .false.
      diagnostics % areaEdgeGlobal % ioinfo % restart = .false.
      diagnostics % areaEdgeGlobal % ioinfo % output = .true.
      diagnostics % areaEdgeGlobal % block => b

      allocate(diagnostics % areaTriangleGlobal)
      allocate(diagnostics % areaTriangleGlobal % ioinfo)
      diagnostics % areaTriangleGlobal % fieldName = 'areaTriangleGlobal'
      diagnostics % areaTriangleGlobal % isVarArray = .false.
      diagnostics % areaTriangleGlobal % hasTimeDimension = .true.
      nullify(diagnostics % areaTriangleGlobal % prev)
      nullify(diagnostics % areaTriangleGlobal % next)
      nullify(diagnostics % areaTriangleGlobal % sendList)
      nullify(diagnostics % areaTriangleGlobal % recvList)
      nullify(diagnostics % areaTriangleGlobal % copyList)
      diagnostics % areaTriangleGlobal % ioinfo % input = .false.
      diagnostics % areaTriangleGlobal % ioinfo % sfc = .false.
      diagnostics % areaTriangleGlobal % ioinfo % restart = .false.
      diagnostics % areaTriangleGlobal % ioinfo % output = .true.
      diagnostics % areaTriangleGlobal % block => b

      allocate(diagnostics % volumeCellGlobal)
      allocate(diagnostics % volumeCellGlobal % ioinfo)
      diagnostics % volumeCellGlobal % fieldName = 'volumeCellGlobal'
      diagnostics % volumeCellGlobal % isVarArray = .false.
      diagnostics % volumeCellGlobal % hasTimeDimension = .true.
      nullify(diagnostics % volumeCellGlobal % prev)
      nullify(diagnostics % volumeCellGlobal % next)
      nullify(diagnostics % volumeCellGlobal % sendList)
      nullify(diagnostics % volumeCellGlobal % recvList)
      nullify(diagnostics % volumeCellGlobal % copyList)
      diagnostics % volumeCellGlobal % ioinfo % input = .false.
      diagnostics % volumeCellGlobal % ioinfo % sfc = .false.
      diagnostics % volumeCellGlobal % ioinfo % restart = .false.
      diagnostics % volumeCellGlobal % ioinfo % output = .true.
      diagnostics % volumeCellGlobal % block => b

      allocate(diagnostics % volumeEdgeGlobal)
      allocate(diagnostics % volumeEdgeGlobal % ioinfo)
      diagnostics % volumeEdgeGlobal % fieldName = 'volumeEdgeGlobal'
      diagnostics % volumeEdgeGlobal % isVarArray = .false.
      diagnostics % volumeEdgeGlobal % hasTimeDimension = .true.
      nullify(diagnostics % volumeEdgeGlobal % prev)
      nullify(diagnostics % volumeEdgeGlobal % next)
      nullify(diagnostics % volumeEdgeGlobal % sendList)
      nullify(diagnostics % volumeEdgeGlobal % recvList)
      nullify(diagnostics % volumeEdgeGlobal % copyList)
      diagnostics % volumeEdgeGlobal % ioinfo % input = .false.
      diagnostics % volumeEdgeGlobal % ioinfo % sfc = .false.
      diagnostics % volumeEdgeGlobal % ioinfo % restart = .false.
      diagnostics % volumeEdgeGlobal % ioinfo % output = .true.
      diagnostics % volumeEdgeGlobal % block => b

      allocate(diagnostics % CFLNumberGlobal)
      allocate(diagnostics % CFLNumberGlobal % ioinfo)
      diagnostics % CFLNumberGlobal % fieldName = 'CFLNumberGlobal'
      diagnostics % CFLNumberGlobal % isVarArray = .false.
      diagnostics % CFLNumberGlobal % hasTimeDimension = .true.
      nullify(diagnostics % CFLNumberGlobal % prev)
      nullify(diagnostics % CFLNumberGlobal % next)
      nullify(diagnostics % CFLNumberGlobal % sendList)
      nullify(diagnostics % CFLNumberGlobal % recvList)
      nullify(diagnostics % CFLNumberGlobal % copyList)
      diagnostics % CFLNumberGlobal % ioinfo % input = .false.
      diagnostics % CFLNumberGlobal % ioinfo % sfc = .false.
      diagnostics % CFLNumberGlobal % ioinfo % restart = .false.
      diagnostics % CFLNumberGlobal % ioinfo % output = .true.
      diagnostics % CFLNumberGlobal % block => b

   end subroutine mpas_allocate_diagnostics


   subroutine mpas_allocate_average(b, average, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (average_type), intent(inout) :: average
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      average % block => b
      allocate(average % nAverage)
      allocate(average % nAverage % ioinfo)
      average % nAverage % fieldName = 'nAverage'
      average % nAverage % isVarArray = .false.
      average % nAverage % hasTimeDimension = .true.
      nullify(average % nAverage % prev)
      nullify(average % nAverage % next)
      nullify(average % nAverage % sendList)
      nullify(average % nAverage % recvList)
      nullify(average % nAverage % copyList)
      average % nAverage % ioinfo % input = .false.
      average % nAverage % ioinfo % sfc = .false.
      average % nAverage % ioinfo % restart = .false.
      average % nAverage % ioinfo % output = .true.
      average % nAverage % block => b

      allocate(average % avgSsh)
      allocate(average % avgSsh % ioinfo)
      average % avgSsh % fieldName = 'avgSsh'
      average % avgSsh % isVarArray = .false.
      average % avgSsh % isPersistent = .true.
      average % avgSsh % isActive = .true.
      allocate(average % avgSsh % array(nCells + 1))
      average % avgSsh % array = 0.0_RKIND
      average % avgSsh % dimSizes(1) = nCells
      average % avgSsh % dimNames(1) = 'nCells'
      average % avgSsh % hasTimeDimension = .true.
      nullify(average % avgSsh % prev)
      nullify(average % avgSsh % next)
      nullify(average % avgSsh % sendList)
      nullify(average % avgSsh % recvList)
      nullify(average % avgSsh % copyList)
      average % avgSsh % ioinfo % input = .false.
      average % avgSsh % ioinfo % sfc = .false.
      average % avgSsh % ioinfo % restart = .false.
      average % avgSsh % ioinfo % output = .true.
      average % avgSsh % block => b

      allocate(average % varSsh)
      allocate(average % varSsh % ioinfo)
      average % varSsh % fieldName = 'varSsh'
      average % varSsh % isVarArray = .false.
      average % varSsh % isPersistent = .true.
      average % varSsh % isActive = .true.
      allocate(average % varSsh % array(nCells + 1))
      average % varSsh % array = 0.0_RKIND
      average % varSsh % dimSizes(1) = nCells
      average % varSsh % dimNames(1) = 'nCells'
      average % varSsh % hasTimeDimension = .true.
      nullify(average % varSsh % prev)
      nullify(average % varSsh % next)
      nullify(average % varSsh % sendList)
      nullify(average % varSsh % recvList)
      nullify(average % varSsh % copyList)
      average % varSsh % ioinfo % input = .false.
      average % varSsh % ioinfo % sfc = .false.
      average % varSsh % ioinfo % restart = .false.
      average % varSsh % ioinfo % output = .true.
      average % varSsh % block => b

      allocate(average % avgVelocityZonal)
      allocate(average % avgVelocityZonal % ioinfo)
      average % avgVelocityZonal % fieldName = 'avgVelocityZonal'
      average % avgVelocityZonal % isVarArray = .false.
      average % avgVelocityZonal % isPersistent = .true.
      average % avgVelocityZonal % isActive = .true.
      allocate(average % avgVelocityZonal % array(nVertLevels, nCells + 1))
      average % avgVelocityZonal % array = 0.0_RKIND
      average % avgVelocityZonal % dimSizes(1) = nVertLevels
      average % avgVelocityZonal % dimNames(1) = 'nVertLevels'
      average % avgVelocityZonal % dimSizes(2) = nCells
      average % avgVelocityZonal % dimNames(2) = 'nCells'
      average % avgVelocityZonal % hasTimeDimension = .true.
      nullify(average % avgVelocityZonal % prev)
      nullify(average % avgVelocityZonal % next)
      nullify(average % avgVelocityZonal % sendList)
      nullify(average % avgVelocityZonal % recvList)
      nullify(average % avgVelocityZonal % copyList)
      average % avgVelocityZonal % ioinfo % input = .false.
      average % avgVelocityZonal % ioinfo % sfc = .false.
      average % avgVelocityZonal % ioinfo % restart = .false.
      average % avgVelocityZonal % ioinfo % output = .true.
      average % avgVelocityZonal % block => b

      allocate(average % avgVelocityMeridional)
      allocate(average % avgVelocityMeridional % ioinfo)
      average % avgVelocityMeridional % fieldName = 'avgVelocityMeridional'
      average % avgVelocityMeridional % isVarArray = .false.
      average % avgVelocityMeridional % isPersistent = .true.
      average % avgVelocityMeridional % isActive = .true.
      allocate(average % avgVelocityMeridional % array(nVertLevels, nCells + 1))
      average % avgVelocityMeridional % array = 0.0_RKIND
      average % avgVelocityMeridional % dimSizes(1) = nVertLevels
      average % avgVelocityMeridional % dimNames(1) = 'nVertLevels'
      average % avgVelocityMeridional % dimSizes(2) = nCells
      average % avgVelocityMeridional % dimNames(2) = 'nCells'
      average % avgVelocityMeridional % hasTimeDimension = .true.
      nullify(average % avgVelocityMeridional % prev)
      nullify(average % avgVelocityMeridional % next)
      nullify(average % avgVelocityMeridional % sendList)
      nullify(average % avgVelocityMeridional % recvList)
      nullify(average % avgVelocityMeridional % copyList)
      average % avgVelocityMeridional % ioinfo % input = .false.
      average % avgVelocityMeridional % ioinfo % sfc = .false.
      average % avgVelocityMeridional % ioinfo % restart = .false.
      average % avgVelocityMeridional % ioinfo % output = .true.
      average % avgVelocityMeridional % block => b

      allocate(average % varVelocityZonal)
      allocate(average % varVelocityZonal % ioinfo)
      average % varVelocityZonal % fieldName = 'varVelocityZonal'
      average % varVelocityZonal % isVarArray = .false.
      average % varVelocityZonal % isPersistent = .true.
      average % varVelocityZonal % isActive = .true.
      allocate(average % varVelocityZonal % array(nVertLevels, nCells + 1))
      average % varVelocityZonal % array = 0.0_RKIND
      average % varVelocityZonal % dimSizes(1) = nVertLevels
      average % varVelocityZonal % dimNames(1) = 'nVertLevels'
      average % varVelocityZonal % dimSizes(2) = nCells
      average % varVelocityZonal % dimNames(2) = 'nCells'
      average % varVelocityZonal % hasTimeDimension = .true.
      nullify(average % varVelocityZonal % prev)
      nullify(average % varVelocityZonal % next)
      nullify(average % varVelocityZonal % sendList)
      nullify(average % varVelocityZonal % recvList)
      nullify(average % varVelocityZonal % copyList)
      average % varVelocityZonal % ioinfo % input = .false.
      average % varVelocityZonal % ioinfo % sfc = .false.
      average % varVelocityZonal % ioinfo % restart = .false.
      average % varVelocityZonal % ioinfo % output = .true.
      average % varVelocityZonal % block => b

      allocate(average % varVelocityMeridional)
      allocate(average % varVelocityMeridional % ioinfo)
      average % varVelocityMeridional % fieldName = 'varVelocityMeridional'
      average % varVelocityMeridional % isVarArray = .false.
      average % varVelocityMeridional % isPersistent = .true.
      average % varVelocityMeridional % isActive = .true.
      allocate(average % varVelocityMeridional % array(nVertLevels, nCells + 1))
      average % varVelocityMeridional % array = 0.0_RKIND
      average % varVelocityMeridional % dimSizes(1) = nVertLevels
      average % varVelocityMeridional % dimNames(1) = 'nVertLevels'
      average % varVelocityMeridional % dimSizes(2) = nCells
      average % varVelocityMeridional % dimNames(2) = 'nCells'
      average % varVelocityMeridional % hasTimeDimension = .true.
      nullify(average % varVelocityMeridional % prev)
      nullify(average % varVelocityMeridional % next)
      nullify(average % varVelocityMeridional % sendList)
      nullify(average % varVelocityMeridional % recvList)
      nullify(average % varVelocityMeridional % copyList)
      average % varVelocityMeridional % ioinfo % input = .false.
      average % varVelocityMeridional % ioinfo % sfc = .false.
      average % varVelocityMeridional % ioinfo % restart = .false.
      average % varVelocityMeridional % ioinfo % output = .true.
      average % varVelocityMeridional % block => b

      allocate(average % avgNormalVelocity)
      allocate(average % avgNormalVelocity % ioinfo)
      average % avgNormalVelocity % fieldName = 'avgNormalVelocity'
      average % avgNormalVelocity % isVarArray = .false.
      average % avgNormalVelocity % isPersistent = .true.
      average % avgNormalVelocity % isActive = .true.
      allocate(average % avgNormalVelocity % array(nVertLevels, nEdges + 1))
      average % avgNormalVelocity % array = 0.0_RKIND
      average % avgNormalVelocity % dimSizes(1) = nVertLevels
      average % avgNormalVelocity % dimNames(1) = 'nVertLevels'
      average % avgNormalVelocity % dimSizes(2) = nEdges
      average % avgNormalVelocity % dimNames(2) = 'nEdges'
      average % avgNormalVelocity % hasTimeDimension = .true.
      nullify(average % avgNormalVelocity % prev)
      nullify(average % avgNormalVelocity % next)
      nullify(average % avgNormalVelocity % sendList)
      nullify(average % avgNormalVelocity % recvList)
      nullify(average % avgNormalVelocity % copyList)
      average % avgNormalVelocity % ioinfo % input = .false.
      average % avgNormalVelocity % ioinfo % sfc = .false.
      average % avgNormalVelocity % ioinfo % restart = .false.
      average % avgNormalVelocity % ioinfo % output = .true.
      average % avgNormalVelocity % block => b

      allocate(average % varNormalVelocity)
      allocate(average % varNormalVelocity % ioinfo)
      average % varNormalVelocity % fieldName = 'varNormalVelocity'
      average % varNormalVelocity % isVarArray = .false.
      average % varNormalVelocity % isPersistent = .true.
      average % varNormalVelocity % isActive = .true.
      allocate(average % varNormalVelocity % array(nVertLevels, nEdges + 1))
      average % varNormalVelocity % array = 0.0_RKIND
      average % varNormalVelocity % dimSizes(1) = nVertLevels
      average % varNormalVelocity % dimNames(1) = 'nVertLevels'
      average % varNormalVelocity % dimSizes(2) = nEdges
      average % varNormalVelocity % dimNames(2) = 'nEdges'
      average % varNormalVelocity % hasTimeDimension = .true.
      nullify(average % varNormalVelocity % prev)
      nullify(average % varNormalVelocity % next)
      nullify(average % varNormalVelocity % sendList)
      nullify(average % varNormalVelocity % recvList)
      nullify(average % varNormalVelocity % copyList)
      average % varNormalVelocity % ioinfo % input = .false.
      average % varNormalVelocity % ioinfo % sfc = .false.
      average % varNormalVelocity % ioinfo % restart = .false.
      average % varNormalVelocity % ioinfo % output = .true.
      average % varNormalVelocity % block => b

      allocate(average % avgVertVelocityTop)
      allocate(average % avgVertVelocityTop % ioinfo)
      average % avgVertVelocityTop % fieldName = 'avgVertVelocityTop'
      average % avgVertVelocityTop % isVarArray = .false.
      average % avgVertVelocityTop % isPersistent = .true.
      average % avgVertVelocityTop % isActive = .true.
      allocate(average % avgVertVelocityTop % array(nVertLevels+1, nCells + 1))
      average % avgVertVelocityTop % array = 0.0_RKIND
      average % avgVertVelocityTop % dimSizes(1) = nVertLevels+1
      average % avgVertVelocityTop % dimNames(1) = 'nVertLevelsP1'
      average % avgVertVelocityTop % dimSizes(2) = nCells
      average % avgVertVelocityTop % dimNames(2) = 'nCells'
      average % avgVertVelocityTop % hasTimeDimension = .true.
      nullify(average % avgVertVelocityTop % prev)
      nullify(average % avgVertVelocityTop % next)
      nullify(average % avgVertVelocityTop % sendList)
      nullify(average % avgVertVelocityTop % recvList)
      nullify(average % avgVertVelocityTop % copyList)
      average % avgVertVelocityTop % ioinfo % input = .false.
      average % avgVertVelocityTop % ioinfo % sfc = .false.
      average % avgVertVelocityTop % ioinfo % restart = .false.
      average % avgVertVelocityTop % ioinfo % output = .true.
      average % avgVertVelocityTop % block => b

   end subroutine mpas_allocate_average


   subroutine mpas_allocate_forcing(b, forcing, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (forcing_type), intent(inout) :: forcing
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      forcing % block => b
      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(forcing % surfaceTracerFlux)
      allocate(forcing % surfaceTracerFlux % ioinfo)
      forcing % surfaceTracerFlux % fieldName = 'surfaceTracerFlux'
      forcing % surfaceTracerFlux % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_surfaceTemperatureFlux = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_surfaceSalinityFlux = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter >= 0) then
         forcing % dynamics_start = group_start
         forcing % dynamics_end = group_start + group_counter
      end if
      group_counter = -1
      group_started = .false.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_surfaceTracer1Flux = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         forcing % testing_start = group_start
         forcing % testing_end = group_start + group_counter
      end if
      forcing % num_surfaceTracerFlux = index_counter
      if ( forcing % num_surfaceTracerFlux > 0 ) then
         allocate(forcing % surfaceTracerFlux % constituentNames(forcing % num_surfaceTracerFlux))
      end if
      forcing % surfaceTracerFlux % constituentNames(forcing % index_surfaceTemperatureFlux) = 'surfaceTemperatureFlux'
      forcing % surfaceTracerFlux % constituentNames(forcing % index_surfaceSalinityFlux) = 'surfaceSalinityFlux'
      forcing % surfaceTracerFlux % constituentNames(forcing % index_surfaceTracer1Flux) = 'surfaceTracer1Flux'
      forcing % surfaceTracerFlux % isPersistent = .true.
      forcing % surfaceTracerFlux % isActive = .true.
      allocate(forcing % surfaceTracerFlux % array(forcing % num_surfaceTracerFlux, nCells + 1))
      forcing % surfaceTracerFlux % array = 0.0_RKIND
      forcing % surfaceTracerFlux % dimSizes(1) = forcing % num_surfaceTracerFlux
      forcing % surfaceTracerFlux % dimNames(1) = 'num_surfaceTracerFlux'
      forcing % surfaceTracerFlux % dimSizes(2) = nCells
      forcing % surfaceTracerFlux % dimNames(2) = 'nCells'
      forcing % surfaceTracerFlux % hasTimeDimension = .true.
      nullify(forcing % surfaceTracerFlux % prev)
      nullify(forcing % surfaceTracerFlux % next)
      nullify(forcing % surfaceTracerFlux % sendList)
      nullify(forcing % surfaceTracerFlux % recvList)
      nullify(forcing % surfaceTracerFlux % copyList)
      forcing % surfaceTracerFlux % ioinfo % input = .false.
      forcing % surfaceTracerFlux % ioinfo % sfc = .false.
      forcing % surfaceTracerFlux % ioinfo % restart = .false.
      forcing % surfaceTracerFlux % ioinfo % output = .true.
      forcing % surfaceTracerFlux % block => b

      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(forcing % avgTracersSurfaceValue)
      allocate(forcing % avgTracersSurfaceValue % ioinfo)
      forcing % avgTracersSurfaceValue % fieldName = 'avgTracersSurfaceValue'
      forcing % avgTracersSurfaceValue % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_temperatureSurfaceValue = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_salinitySurfaceValue = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_avgTracer1SurfaceValue = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         forcing % surfaceValues_start = group_start
         forcing % surfaceValues_end = group_start + group_counter
      end if
      forcing % num_avgTracersSurfaceValue = index_counter
      if ( forcing % num_avgTracersSurfaceValue > 0 ) then
         allocate(forcing % avgTracersSurfaceValue % constituentNames(forcing % num_avgTracersSurfaceValue))
      end if
      forcing % avgTracersSurfaceValue % constituentNames(forcing % index_temperatureSurfaceValue) = 'avgTemperatureSurfaceValue'
      forcing % avgTracersSurfaceValue % constituentNames(forcing % index_salinitySurfaceValue) = 'avgSalinitySurfaceValue'
      forcing % avgTracersSurfaceValue % constituentNames(forcing % index_avgTracer1SurfaceValue) = 'avgTracer1SurfaceValue'
      forcing % avgTracersSurfaceValue % isPersistent = .true.
      forcing % avgTracersSurfaceValue % isActive = .true.
      allocate(forcing % avgTracersSurfaceValue % array(forcing % num_avgTracersSurfaceValue, nCells + 1))
      forcing % avgTracersSurfaceValue % array = 0.0_RKIND
      forcing % avgTracersSurfaceValue % dimSizes(1) = forcing % num_avgTracersSurfaceValue
      forcing % avgTracersSurfaceValue % dimNames(1) = 'num_avgTracersSurfaceValue'
      forcing % avgTracersSurfaceValue % dimSizes(2) = nCells
      forcing % avgTracersSurfaceValue % dimNames(2) = 'nCells'
      forcing % avgTracersSurfaceValue % hasTimeDimension = .true.
      nullify(forcing % avgTracersSurfaceValue % prev)
      nullify(forcing % avgTracersSurfaceValue % next)
      nullify(forcing % avgTracersSurfaceValue % sendList)
      nullify(forcing % avgTracersSurfaceValue % recvList)
      nullify(forcing % avgTracersSurfaceValue % copyList)
      forcing % avgTracersSurfaceValue % ioinfo % input = .false.
      forcing % avgTracersSurfaceValue % ioinfo % sfc = .false.
      forcing % avgTracersSurfaceValue % ioinfo % restart = .false.
      forcing % avgTracersSurfaceValue % ioinfo % output = .false.
      forcing % avgTracersSurfaceValue % block => b

      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(forcing % avgSurfaceVelocity)
      allocate(forcing % avgSurfaceVelocity % ioinfo)
      forcing % avgSurfaceVelocity % fieldName = 'avgSurfaceVelocity'
      forcing % avgSurfaceVelocity % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_avgZonalSurfaceVelocity = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter >= 0) then
         forcing % vel_zonal_start = group_start
         forcing % vel_zonal_end = group_start + group_counter
      end if
      group_counter = -1
      group_started = .false.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_avgMeridionalSurfaceVelocity = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         forcing % vel_meridional_start = group_start
         forcing % vel_meridional_end = group_start + group_counter
      end if
      forcing % num_avgSurfaceVelocity = index_counter
      if ( forcing % num_avgSurfaceVelocity > 0 ) then
         allocate(forcing % avgSurfaceVelocity % constituentNames(forcing % num_avgSurfaceVelocity))
      end if
      forcing % avgSurfaceVelocity % constituentNames(forcing % index_avgZonalSurfaceVelocity) = 'avgZonalSurfaceVelocity'
      forcing % avgSurfaceVelocity % constituentNames(forcing % index_avgMeridionalSurfaceVelocity) = &
'avgMeridionalSurfaceVelocity'
      forcing % avgSurfaceVelocity % isPersistent = .true.
      forcing % avgSurfaceVelocity % isActive = .true.
      allocate(forcing % avgSurfaceVelocity % array(forcing % num_avgSurfaceVelocity, nCells + 1))
      forcing % avgSurfaceVelocity % array = 0.0_RKIND
      forcing % avgSurfaceVelocity % dimSizes(1) = forcing % num_avgSurfaceVelocity
      forcing % avgSurfaceVelocity % dimNames(1) = 'num_avgSurfaceVelocity'
      forcing % avgSurfaceVelocity % dimSizes(2) = nCells
      forcing % avgSurfaceVelocity % dimNames(2) = 'nCells'
      forcing % avgSurfaceVelocity % hasTimeDimension = .true.
      nullify(forcing % avgSurfaceVelocity % prev)
      nullify(forcing % avgSurfaceVelocity % next)
      nullify(forcing % avgSurfaceVelocity % sendList)
      nullify(forcing % avgSurfaceVelocity % recvList)
      nullify(forcing % avgSurfaceVelocity % copyList)
      forcing % avgSurfaceVelocity % ioinfo % input = .false.
      forcing % avgSurfaceVelocity % ioinfo % sfc = .false.
      forcing % avgSurfaceVelocity % ioinfo % restart = .false.
      forcing % avgSurfaceVelocity % ioinfo % output = .true.
      forcing % avgSurfaceVelocity % block => b

      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.
      allocate(forcing % avgSSHGradient)
      allocate(forcing % avgSSHGradient % ioinfo)
      forcing % avgSSHGradient % fieldName = 'avgSSHGradient'
      forcing % avgSSHGradient % isVarArray = .true.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_avgZonalSSHGradient = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter >= 0) then
         forcing % ssh_zonal_start = group_start
         forcing % ssh_zonal_end = group_start + group_counter
      end if
      group_counter = -1
      group_started = .false.
      index_counter = index_counter + 1
      group_counter = group_counter + 1
      forcing % index_avgMeridionalSSHGradient = index_counter
      if (.not. group_started) then
         group_start = index_counter
         group_started = .true.
      end if
      if (group_counter > 0) then
         forcing % ssh_meridional_start = group_start
         forcing % ssh_meridional_end = group_start + group_counter
      end if
      forcing % num_avgSSHGradient = index_counter
      if ( forcing % num_avgSSHGradient > 0 ) then
         allocate(forcing % avgSSHGradient % constituentNames(forcing % num_avgSSHGradient))
      end if
      forcing % avgSSHGradient % constituentNames(forcing % index_avgZonalSSHGradient) = 'avgZonalSSHGradient'
      forcing % avgSSHGradient % constituentNames(forcing % index_avgMeridionalSSHGradient) = 'avgMeridionalSSHGradient'
      forcing % avgSSHGradient % isPersistent = .true.
      forcing % avgSSHGradient % isActive = .true.
      allocate(forcing % avgSSHGradient % array(forcing % num_avgSSHGradient, nCells + 1))
      forcing % avgSSHGradient % array = 0.0_RKIND
      forcing % avgSSHGradient % dimSizes(1) = forcing % num_avgSSHGradient
      forcing % avgSSHGradient % dimNames(1) = 'num_avgSSHGradient'
      forcing % avgSSHGradient % dimSizes(2) = nCells
      forcing % avgSSHGradient % dimNames(2) = 'nCells'
      forcing % avgSSHGradient % hasTimeDimension = .true.
      nullify(forcing % avgSSHGradient % prev)
      nullify(forcing % avgSSHGradient % next)
      nullify(forcing % avgSSHGradient % sendList)
      nullify(forcing % avgSSHGradient % recvList)
      nullify(forcing % avgSSHGradient % copyList)
      forcing % avgSSHGradient % ioinfo % input = .false.
      forcing % avgSSHGradient % ioinfo % sfc = .false.
      forcing % avgSSHGradient % ioinfo % restart = .false.
      forcing % avgSSHGradient % ioinfo % output = .true.
      forcing % avgSSHGradient % block => b

      allocate(forcing % surfaceWindStress)
      allocate(forcing % surfaceWindStress % ioinfo)
      forcing % surfaceWindStress % fieldName = 'surfaceWindStress'
      forcing % surfaceWindStress % isVarArray = .false.
      forcing % surfaceWindStress % isPersistent = .true.
      forcing % surfaceWindStress % isActive = .true.
      allocate(forcing % surfaceWindStress % array(nEdges + 1))
      forcing % surfaceWindStress % array = 0.0_RKIND
      forcing % surfaceWindStress % dimSizes(1) = nEdges
      forcing % surfaceWindStress % dimNames(1) = 'nEdges'
      forcing % surfaceWindStress % hasTimeDimension = .true.
      nullify(forcing % surfaceWindStress % prev)
      nullify(forcing % surfaceWindStress % next)
      nullify(forcing % surfaceWindStress % sendList)
      nullify(forcing % surfaceWindStress % recvList)
      nullify(forcing % surfaceWindStress % copyList)
      forcing % surfaceWindStress % ioinfo % input = .true.
      forcing % surfaceWindStress % ioinfo % sfc = .false.
      forcing % surfaceWindStress % ioinfo % restart = .true.
      forcing % surfaceWindStress % ioinfo % output = .false.
      forcing % surfaceWindStress % block => b

      allocate(forcing % surfaceWindStressMagnitude)
      allocate(forcing % surfaceWindStressMagnitude % ioinfo)
      forcing % surfaceWindStressMagnitude % fieldName = 'surfaceWindStressMagnitude'
      forcing % surfaceWindStressMagnitude % isVarArray = .false.
      forcing % surfaceWindStressMagnitude % isPersistent = .true.
      forcing % surfaceWindStressMagnitude % isActive = .true.
      allocate(forcing % surfaceWindStressMagnitude % array(nCells + 1))
      forcing % surfaceWindStressMagnitude % array = 0.0_RKIND
      forcing % surfaceWindStressMagnitude % dimSizes(1) = nCells
      forcing % surfaceWindStressMagnitude % dimNames(1) = 'nCells'
      forcing % surfaceWindStressMagnitude % hasTimeDimension = .true.
      nullify(forcing % surfaceWindStressMagnitude % prev)
      nullify(forcing % surfaceWindStressMagnitude % next)
      nullify(forcing % surfaceWindStressMagnitude % sendList)
      nullify(forcing % surfaceWindStressMagnitude % recvList)
      nullify(forcing % surfaceWindStressMagnitude % copyList)
      forcing % surfaceWindStressMagnitude % ioinfo % input = .false.
      forcing % surfaceWindStressMagnitude % ioinfo % sfc = .false.
      forcing % surfaceWindStressMagnitude % ioinfo % restart = .false.
      forcing % surfaceWindStressMagnitude % ioinfo % output = .true.
      forcing % surfaceWindStressMagnitude % block => b

      allocate(forcing % surfaceMassFlux)
      allocate(forcing % surfaceMassFlux % ioinfo)
      forcing % surfaceMassFlux % fieldName = 'surfaceMassFlux'
      forcing % surfaceMassFlux % isVarArray = .false.
      forcing % surfaceMassFlux % isPersistent = .true.
      forcing % surfaceMassFlux % isActive = .true.
      allocate(forcing % surfaceMassFlux % array(nCells + 1))
      forcing % surfaceMassFlux % array = 0.0_RKIND
      forcing % surfaceMassFlux % dimSizes(1) = nCells
      forcing % surfaceMassFlux % dimNames(1) = 'nCells'
      forcing % surfaceMassFlux % hasTimeDimension = .true.
      nullify(forcing % surfaceMassFlux % prev)
      nullify(forcing % surfaceMassFlux % next)
      nullify(forcing % surfaceMassFlux % sendList)
      nullify(forcing % surfaceMassFlux % recvList)
      nullify(forcing % surfaceMassFlux % copyList)
      forcing % surfaceMassFlux % ioinfo % input = .false.
      forcing % surfaceMassFlux % ioinfo % sfc = .false.
      forcing % surfaceMassFlux % ioinfo % restart = .false.
      forcing % surfaceMassFlux % ioinfo % output = .true.
      forcing % surfaceMassFlux % block => b

      allocate(forcing % seaSurfacePressure)
      allocate(forcing % seaSurfacePressure % ioinfo)
      forcing % seaSurfacePressure % fieldName = 'seaSurfacePressure'
      forcing % seaSurfacePressure % isVarArray = .false.
      forcing % seaSurfacePressure % isPersistent = .true.
      forcing % seaSurfacePressure % isActive = .true.
      allocate(forcing % seaSurfacePressure % array(nCells + 1))
      forcing % seaSurfacePressure % array = 0.0_RKIND
      forcing % seaSurfacePressure % dimSizes(1) = nCells
      forcing % seaSurfacePressure % dimNames(1) = 'nCells'
      forcing % seaSurfacePressure % hasTimeDimension = .true.
      nullify(forcing % seaSurfacePressure % prev)
      nullify(forcing % seaSurfacePressure % next)
      nullify(forcing % seaSurfacePressure % sendList)
      nullify(forcing % seaSurfacePressure % recvList)
      nullify(forcing % seaSurfacePressure % copyList)
      forcing % seaSurfacePressure % ioinfo % input = .false.
      forcing % seaSurfacePressure % ioinfo % sfc = .false.
      forcing % seaSurfacePressure % ioinfo % restart = .true.
      forcing % seaSurfacePressure % ioinfo % output = .true.
      forcing % seaSurfacePressure % block => b

      allocate(forcing % seaIceEnergy)
      allocate(forcing % seaIceEnergy % ioinfo)
      forcing % seaIceEnergy % fieldName = 'seaIceEnergy'
      forcing % seaIceEnergy % isVarArray = .false.
      forcing % seaIceEnergy % isPersistent = .true.
      forcing % seaIceEnergy % isActive = .true.
      allocate(forcing % seaIceEnergy % array(nCells + 1))
      forcing % seaIceEnergy % array = 0.0_RKIND
      forcing % seaIceEnergy % dimSizes(1) = nCells
      forcing % seaIceEnergy % dimNames(1) = 'nCells'
      forcing % seaIceEnergy % hasTimeDimension = .true.
      nullify(forcing % seaIceEnergy % prev)
      nullify(forcing % seaIceEnergy % next)
      nullify(forcing % seaIceEnergy % sendList)
      nullify(forcing % seaIceEnergy % recvList)
      nullify(forcing % seaIceEnergy % copyList)
      forcing % seaIceEnergy % ioinfo % input = .false.
      forcing % seaIceEnergy % ioinfo % sfc = .false.
      forcing % seaIceEnergy % ioinfo % restart = .false.
      forcing % seaIceEnergy % ioinfo % output = .true.
      forcing % seaIceEnergy % block => b

      allocate(forcing % penetrativeTemperatureFlux)
      allocate(forcing % penetrativeTemperatureFlux % ioinfo)
      forcing % penetrativeTemperatureFlux % fieldName = 'penetrativeTemperatureFlux'
      forcing % penetrativeTemperatureFlux % isVarArray = .false.
      forcing % penetrativeTemperatureFlux % isPersistent = .true.
      forcing % penetrativeTemperatureFlux % isActive = .true.
      allocate(forcing % penetrativeTemperatureFlux % array(nCells + 1))
      forcing % penetrativeTemperatureFlux % array = 0.0_RKIND
      forcing % penetrativeTemperatureFlux % dimSizes(1) = nCells
      forcing % penetrativeTemperatureFlux % dimNames(1) = 'nCells'
      forcing % penetrativeTemperatureFlux % hasTimeDimension = .true.
      nullify(forcing % penetrativeTemperatureFlux % prev)
      nullify(forcing % penetrativeTemperatureFlux % next)
      nullify(forcing % penetrativeTemperatureFlux % sendList)
      nullify(forcing % penetrativeTemperatureFlux % recvList)
      nullify(forcing % penetrativeTemperatureFlux % copyList)
      forcing % penetrativeTemperatureFlux % ioinfo % input = .false.
      forcing % penetrativeTemperatureFlux % ioinfo % sfc = .false.
      forcing % penetrativeTemperatureFlux % ioinfo % restart = .false.
      forcing % penetrativeTemperatureFlux % ioinfo % output = .true.
      forcing % penetrativeTemperatureFlux % block => b

      allocate(forcing % transmissionCoefficients)
      allocate(forcing % transmissionCoefficients % ioinfo)
      forcing % transmissionCoefficients % fieldName = 'transmissionCoefficients'
      forcing % transmissionCoefficients % isVarArray = .false.
      forcing % transmissionCoefficients % isPersistent = .true.
      forcing % transmissionCoefficients % isActive = .true.
      allocate(forcing % transmissionCoefficients % array(nVertLevels, nCells + 1))
      forcing % transmissionCoefficients % array = 0.0_RKIND
      forcing % transmissionCoefficients % dimSizes(1) = nVertLevels
      forcing % transmissionCoefficients % dimNames(1) = 'nVertLevels'
      forcing % transmissionCoefficients % dimSizes(2) = nCells
      forcing % transmissionCoefficients % dimNames(2) = 'nCells'
      forcing % transmissionCoefficients % hasTimeDimension = .true.
      nullify(forcing % transmissionCoefficients % prev)
      nullify(forcing % transmissionCoefficients % next)
      nullify(forcing % transmissionCoefficients % sendList)
      nullify(forcing % transmissionCoefficients % recvList)
      nullify(forcing % transmissionCoefficients % copyList)
      forcing % transmissionCoefficients % ioinfo % input = .false.
      forcing % transmissionCoefficients % ioinfo % sfc = .false.
      forcing % transmissionCoefficients % ioinfo % restart = .false.
      forcing % transmissionCoefficients % ioinfo % output = .true.
      forcing % transmissionCoefficients % block => b

      allocate(forcing % windStressZonal)
      allocate(forcing % windStressZonal % ioinfo)
      forcing % windStressZonal % fieldName = 'windStressZonal'
      forcing % windStressZonal % isVarArray = .false.
      forcing % windStressZonal % isPersistent = .true.
      forcing % windStressZonal % isActive = .true.
      allocate(forcing % windStressZonal % array(nCells + 1))
      forcing % windStressZonal % array = 0.0_RKIND
      forcing % windStressZonal % dimSizes(1) = nCells
      forcing % windStressZonal % dimNames(1) = 'nCells'
      forcing % windStressZonal % hasTimeDimension = .true.
      nullify(forcing % windStressZonal % prev)
      nullify(forcing % windStressZonal % next)
      nullify(forcing % windStressZonal % sendList)
      nullify(forcing % windStressZonal % recvList)
      nullify(forcing % windStressZonal % copyList)
      forcing % windStressZonal % ioinfo % input = .false.
      forcing % windStressZonal % ioinfo % sfc = .false.
      forcing % windStressZonal % ioinfo % restart = .false.
      forcing % windStressZonal % ioinfo % output = .true.
      forcing % windStressZonal % block => b

      allocate(forcing % windStressMeridional)
      allocate(forcing % windStressMeridional % ioinfo)
      forcing % windStressMeridional % fieldName = 'windStressMeridional'
      forcing % windStressMeridional % isVarArray = .false.
      forcing % windStressMeridional % isPersistent = .true.
      forcing % windStressMeridional % isActive = .true.
      allocate(forcing % windStressMeridional % array(nCells + 1))
      forcing % windStressMeridional % array = 0.0_RKIND
      forcing % windStressMeridional % dimSizes(1) = nCells
      forcing % windStressMeridional % dimNames(1) = 'nCells'
      forcing % windStressMeridional % hasTimeDimension = .true.
      nullify(forcing % windStressMeridional % prev)
      nullify(forcing % windStressMeridional % next)
      nullify(forcing % windStressMeridional % sendList)
      nullify(forcing % windStressMeridional % recvList)
      nullify(forcing % windStressMeridional % copyList)
      forcing % windStressMeridional % ioinfo % input = .false.
      forcing % windStressMeridional % ioinfo % sfc = .false.
      forcing % windStressMeridional % ioinfo % restart = .false.
      forcing % windStressMeridional % ioinfo % output = .true.
      forcing % windStressMeridional % block => b

      allocate(forcing % latentHeatFlux)
      allocate(forcing % latentHeatFlux % ioinfo)
      forcing % latentHeatFlux % fieldName = 'latentHeatFlux'
      forcing % latentHeatFlux % isVarArray = .false.
      forcing % latentHeatFlux % isPersistent = .true.
      forcing % latentHeatFlux % isActive = .true.
      allocate(forcing % latentHeatFlux % array(nCells + 1))
      forcing % latentHeatFlux % array = 0.0_RKIND
      forcing % latentHeatFlux % dimSizes(1) = nCells
      forcing % latentHeatFlux % dimNames(1) = 'nCells'
      forcing % latentHeatFlux % hasTimeDimension = .true.
      nullify(forcing % latentHeatFlux % prev)
      nullify(forcing % latentHeatFlux % next)
      nullify(forcing % latentHeatFlux % sendList)
      nullify(forcing % latentHeatFlux % recvList)
      nullify(forcing % latentHeatFlux % copyList)
      forcing % latentHeatFlux % ioinfo % input = .false.
      forcing % latentHeatFlux % ioinfo % sfc = .false.
      forcing % latentHeatFlux % ioinfo % restart = .false.
      forcing % latentHeatFlux % ioinfo % output = .true.
      forcing % latentHeatFlux % block => b

      allocate(forcing % sensibleHeatFlux)
      allocate(forcing % sensibleHeatFlux % ioinfo)
      forcing % sensibleHeatFlux % fieldName = 'sensibleHeatFlux'
      forcing % sensibleHeatFlux % isVarArray = .false.
      forcing % sensibleHeatFlux % isPersistent = .true.
      forcing % sensibleHeatFlux % isActive = .true.
      allocate(forcing % sensibleHeatFlux % array(nCells + 1))
      forcing % sensibleHeatFlux % array = 0.0_RKIND
      forcing % sensibleHeatFlux % dimSizes(1) = nCells
      forcing % sensibleHeatFlux % dimNames(1) = 'nCells'
      forcing % sensibleHeatFlux % hasTimeDimension = .true.
      nullify(forcing % sensibleHeatFlux % prev)
      nullify(forcing % sensibleHeatFlux % next)
      nullify(forcing % sensibleHeatFlux % sendList)
      nullify(forcing % sensibleHeatFlux % recvList)
      nullify(forcing % sensibleHeatFlux % copyList)
      forcing % sensibleHeatFlux % ioinfo % input = .false.
      forcing % sensibleHeatFlux % ioinfo % sfc = .false.
      forcing % sensibleHeatFlux % ioinfo % restart = .false.
      forcing % sensibleHeatFlux % ioinfo % output = .true.
      forcing % sensibleHeatFlux % block => b

      allocate(forcing % longWaveHeatFluxUp)
      allocate(forcing % longWaveHeatFluxUp % ioinfo)
      forcing % longWaveHeatFluxUp % fieldName = 'longWaveHeatFluxUp'
      forcing % longWaveHeatFluxUp % isVarArray = .false.
      forcing % longWaveHeatFluxUp % isPersistent = .true.
      forcing % longWaveHeatFluxUp % isActive = .true.
      allocate(forcing % longWaveHeatFluxUp % array(nCells + 1))
      forcing % longWaveHeatFluxUp % array = 0.0_RKIND
      forcing % longWaveHeatFluxUp % dimSizes(1) = nCells
      forcing % longWaveHeatFluxUp % dimNames(1) = 'nCells'
      forcing % longWaveHeatFluxUp % hasTimeDimension = .true.
      nullify(forcing % longWaveHeatFluxUp % prev)
      nullify(forcing % longWaveHeatFluxUp % next)
      nullify(forcing % longWaveHeatFluxUp % sendList)
      nullify(forcing % longWaveHeatFluxUp % recvList)
      nullify(forcing % longWaveHeatFluxUp % copyList)
      forcing % longWaveHeatFluxUp % ioinfo % input = .false.
      forcing % longWaveHeatFluxUp % ioinfo % sfc = .false.
      forcing % longWaveHeatFluxUp % ioinfo % restart = .false.
      forcing % longWaveHeatFluxUp % ioinfo % output = .true.
      forcing % longWaveHeatFluxUp % block => b

      allocate(forcing % longWaveHeatFluxDown)
      allocate(forcing % longWaveHeatFluxDown % ioinfo)
      forcing % longWaveHeatFluxDown % fieldName = 'longWaveHeatFluxDown'
      forcing % longWaveHeatFluxDown % isVarArray = .false.
      forcing % longWaveHeatFluxDown % isPersistent = .true.
      forcing % longWaveHeatFluxDown % isActive = .true.
      allocate(forcing % longWaveHeatFluxDown % array(nCells + 1))
      forcing % longWaveHeatFluxDown % array = 0.0_RKIND
      forcing % longWaveHeatFluxDown % dimSizes(1) = nCells
      forcing % longWaveHeatFluxDown % dimNames(1) = 'nCells'
      forcing % longWaveHeatFluxDown % hasTimeDimension = .true.
      nullify(forcing % longWaveHeatFluxDown % prev)
      nullify(forcing % longWaveHeatFluxDown % next)
      nullify(forcing % longWaveHeatFluxDown % sendList)
      nullify(forcing % longWaveHeatFluxDown % recvList)
      nullify(forcing % longWaveHeatFluxDown % copyList)
      forcing % longWaveHeatFluxDown % ioinfo % input = .false.
      forcing % longWaveHeatFluxDown % ioinfo % sfc = .false.
      forcing % longWaveHeatFluxDown % ioinfo % restart = .false.
      forcing % longWaveHeatFluxDown % ioinfo % output = .true.
      forcing % longWaveHeatFluxDown % block => b

      allocate(forcing % seaIceHeatFlux)
      allocate(forcing % seaIceHeatFlux % ioinfo)
      forcing % seaIceHeatFlux % fieldName = 'seaIceHeatFlux'
      forcing % seaIceHeatFlux % isVarArray = .false.
      forcing % seaIceHeatFlux % isPersistent = .true.
      forcing % seaIceHeatFlux % isActive = .true.
      allocate(forcing % seaIceHeatFlux % array(nCells + 1))
      forcing % seaIceHeatFlux % array = 0.0_RKIND
      forcing % seaIceHeatFlux % dimSizes(1) = nCells
      forcing % seaIceHeatFlux % dimNames(1) = 'nCells'
      forcing % seaIceHeatFlux % hasTimeDimension = .true.
      nullify(forcing % seaIceHeatFlux % prev)
      nullify(forcing % seaIceHeatFlux % next)
      nullify(forcing % seaIceHeatFlux % sendList)
      nullify(forcing % seaIceHeatFlux % recvList)
      nullify(forcing % seaIceHeatFlux % copyList)
      forcing % seaIceHeatFlux % ioinfo % input = .false.
      forcing % seaIceHeatFlux % ioinfo % sfc = .false.
      forcing % seaIceHeatFlux % ioinfo % restart = .false.
      forcing % seaIceHeatFlux % ioinfo % output = .true.
      forcing % seaIceHeatFlux % block => b

      allocate(forcing % shortWaveHeatFlux)
      allocate(forcing % shortWaveHeatFlux % ioinfo)
      forcing % shortWaveHeatFlux % fieldName = 'shortWaveHeatFlux'
      forcing % shortWaveHeatFlux % isVarArray = .false.
      forcing % shortWaveHeatFlux % isPersistent = .true.
      forcing % shortWaveHeatFlux % isActive = .true.
      allocate(forcing % shortWaveHeatFlux % array(nCells + 1))
      forcing % shortWaveHeatFlux % array = 0.0_RKIND
      forcing % shortWaveHeatFlux % dimSizes(1) = nCells
      forcing % shortWaveHeatFlux % dimNames(1) = 'nCells'
      forcing % shortWaveHeatFlux % hasTimeDimension = .true.
      nullify(forcing % shortWaveHeatFlux % prev)
      nullify(forcing % shortWaveHeatFlux % next)
      nullify(forcing % shortWaveHeatFlux % sendList)
      nullify(forcing % shortWaveHeatFlux % recvList)
      nullify(forcing % shortWaveHeatFlux % copyList)
      forcing % shortWaveHeatFlux % ioinfo % input = .false.
      forcing % shortWaveHeatFlux % ioinfo % sfc = .false.
      forcing % shortWaveHeatFlux % ioinfo % restart = .false.
      forcing % shortWaveHeatFlux % ioinfo % output = .true.
      forcing % shortWaveHeatFlux % block => b

      allocate(forcing % evaporationFlux)
      allocate(forcing % evaporationFlux % ioinfo)
      forcing % evaporationFlux % fieldName = 'evaporationFlux'
      forcing % evaporationFlux % isVarArray = .false.
      forcing % evaporationFlux % isPersistent = .true.
      forcing % evaporationFlux % isActive = .true.
      allocate(forcing % evaporationFlux % array(nCells + 1))
      forcing % evaporationFlux % array = 0.0_RKIND
      forcing % evaporationFlux % dimSizes(1) = nCells
      forcing % evaporationFlux % dimNames(1) = 'nCells'
      forcing % evaporationFlux % hasTimeDimension = .true.
      nullify(forcing % evaporationFlux % prev)
      nullify(forcing % evaporationFlux % next)
      nullify(forcing % evaporationFlux % sendList)
      nullify(forcing % evaporationFlux % recvList)
      nullify(forcing % evaporationFlux % copyList)
      forcing % evaporationFlux % ioinfo % input = .false.
      forcing % evaporationFlux % ioinfo % sfc = .false.
      forcing % evaporationFlux % ioinfo % restart = .false.
      forcing % evaporationFlux % ioinfo % output = .true.
      forcing % evaporationFlux % block => b

      allocate(forcing % seaIceSalinityFlux)
      allocate(forcing % seaIceSalinityFlux % ioinfo)
      forcing % seaIceSalinityFlux % fieldName = 'seaIceSalinityFlux'
      forcing % seaIceSalinityFlux % isVarArray = .false.
      forcing % seaIceSalinityFlux % isPersistent = .true.
      forcing % seaIceSalinityFlux % isActive = .true.
      allocate(forcing % seaIceSalinityFlux % array(nCells + 1))
      forcing % seaIceSalinityFlux % array = 0.0_RKIND
      forcing % seaIceSalinityFlux % dimSizes(1) = nCells
      forcing % seaIceSalinityFlux % dimNames(1) = 'nCells'
      forcing % seaIceSalinityFlux % hasTimeDimension = .true.
      nullify(forcing % seaIceSalinityFlux % prev)
      nullify(forcing % seaIceSalinityFlux % next)
      nullify(forcing % seaIceSalinityFlux % sendList)
      nullify(forcing % seaIceSalinityFlux % recvList)
      nullify(forcing % seaIceSalinityFlux % copyList)
      forcing % seaIceSalinityFlux % ioinfo % input = .false.
      forcing % seaIceSalinityFlux % ioinfo % sfc = .false.
      forcing % seaIceSalinityFlux % ioinfo % restart = .false.
      forcing % seaIceSalinityFlux % ioinfo % output = .true.
      forcing % seaIceSalinityFlux % block => b

      allocate(forcing % seaIceFreshWaterFlux)
      allocate(forcing % seaIceFreshWaterFlux % ioinfo)
      forcing % seaIceFreshWaterFlux % fieldName = 'seaIceFreshWaterFlux'
      forcing % seaIceFreshWaterFlux % isVarArray = .false.
      forcing % seaIceFreshWaterFlux % isPersistent = .true.
      forcing % seaIceFreshWaterFlux % isActive = .true.
      allocate(forcing % seaIceFreshWaterFlux % array(nCells + 1))
      forcing % seaIceFreshWaterFlux % array = 0.0_RKIND
      forcing % seaIceFreshWaterFlux % dimSizes(1) = nCells
      forcing % seaIceFreshWaterFlux % dimNames(1) = 'nCells'
      forcing % seaIceFreshWaterFlux % hasTimeDimension = .true.
      nullify(forcing % seaIceFreshWaterFlux % prev)
      nullify(forcing % seaIceFreshWaterFlux % next)
      nullify(forcing % seaIceFreshWaterFlux % sendList)
      nullify(forcing % seaIceFreshWaterFlux % recvList)
      nullify(forcing % seaIceFreshWaterFlux % copyList)
      forcing % seaIceFreshWaterFlux % ioinfo % input = .false.
      forcing % seaIceFreshWaterFlux % ioinfo % sfc = .false.
      forcing % seaIceFreshWaterFlux % ioinfo % restart = .false.
      forcing % seaIceFreshWaterFlux % ioinfo % output = .true.
      forcing % seaIceFreshWaterFlux % block => b

      allocate(forcing % riverRunoffFlux)
      allocate(forcing % riverRunoffFlux % ioinfo)
      forcing % riverRunoffFlux % fieldName = 'riverRunoffFlux'
      forcing % riverRunoffFlux % isVarArray = .false.
      forcing % riverRunoffFlux % isPersistent = .true.
      forcing % riverRunoffFlux % isActive = .true.
      allocate(forcing % riverRunoffFlux % array(nCells + 1))
      forcing % riverRunoffFlux % array = 0.0_RKIND
      forcing % riverRunoffFlux % dimSizes(1) = nCells
      forcing % riverRunoffFlux % dimNames(1) = 'nCells'
      forcing % riverRunoffFlux % hasTimeDimension = .true.
      nullify(forcing % riverRunoffFlux % prev)
      nullify(forcing % riverRunoffFlux % next)
      nullify(forcing % riverRunoffFlux % sendList)
      nullify(forcing % riverRunoffFlux % recvList)
      nullify(forcing % riverRunoffFlux % copyList)
      forcing % riverRunoffFlux % ioinfo % input = .false.
      forcing % riverRunoffFlux % ioinfo % sfc = .false.
      forcing % riverRunoffFlux % ioinfo % restart = .false.
      forcing % riverRunoffFlux % ioinfo % output = .true.
      forcing % riverRunoffFlux % block => b

      allocate(forcing % iceRunoffFlux)
      allocate(forcing % iceRunoffFlux % ioinfo)
      forcing % iceRunoffFlux % fieldName = 'iceRunoffFlux'
      forcing % iceRunoffFlux % isVarArray = .false.
      forcing % iceRunoffFlux % isPersistent = .true.
      forcing % iceRunoffFlux % isActive = .true.
      allocate(forcing % iceRunoffFlux % array(nCells + 1))
      forcing % iceRunoffFlux % array = 0.0_RKIND
      forcing % iceRunoffFlux % dimSizes(1) = nCells
      forcing % iceRunoffFlux % dimNames(1) = 'nCells'
      forcing % iceRunoffFlux % hasTimeDimension = .true.
      nullify(forcing % iceRunoffFlux % prev)
      nullify(forcing % iceRunoffFlux % next)
      nullify(forcing % iceRunoffFlux % sendList)
      nullify(forcing % iceRunoffFlux % recvList)
      nullify(forcing % iceRunoffFlux % copyList)
      forcing % iceRunoffFlux % ioinfo % input = .false.
      forcing % iceRunoffFlux % ioinfo % sfc = .false.
      forcing % iceRunoffFlux % ioinfo % restart = .false.
      forcing % iceRunoffFlux % ioinfo % output = .true.
      forcing % iceRunoffFlux % block => b

      allocate(forcing % rainFlux)
      allocate(forcing % rainFlux % ioinfo)
      forcing % rainFlux % fieldName = 'rainFlux'
      forcing % rainFlux % isVarArray = .false.
      forcing % rainFlux % isPersistent = .true.
      forcing % rainFlux % isActive = .true.
      allocate(forcing % rainFlux % array(nCells + 1))
      forcing % rainFlux % array = 0.0_RKIND
      forcing % rainFlux % dimSizes(1) = nCells
      forcing % rainFlux % dimNames(1) = 'nCells'
      forcing % rainFlux % hasTimeDimension = .true.
      nullify(forcing % rainFlux % prev)
      nullify(forcing % rainFlux % next)
      nullify(forcing % rainFlux % sendList)
      nullify(forcing % rainFlux % recvList)
      nullify(forcing % rainFlux % copyList)
      forcing % rainFlux % ioinfo % input = .false.
      forcing % rainFlux % ioinfo % sfc = .false.
      forcing % rainFlux % ioinfo % restart = .false.
      forcing % rainFlux % ioinfo % output = .true.
      forcing % rainFlux % block => b

      allocate(forcing % snowFlux)
      allocate(forcing % snowFlux % ioinfo)
      forcing % snowFlux % fieldName = 'snowFlux'
      forcing % snowFlux % isVarArray = .false.
      forcing % snowFlux % isPersistent = .true.
      forcing % snowFlux % isActive = .true.
      allocate(forcing % snowFlux % array(nCells + 1))
      forcing % snowFlux % array = 0.0_RKIND
      forcing % snowFlux % dimSizes(1) = nCells
      forcing % snowFlux % dimNames(1) = 'nCells'
      forcing % snowFlux % hasTimeDimension = .true.
      nullify(forcing % snowFlux % prev)
      nullify(forcing % snowFlux % next)
      nullify(forcing % snowFlux % sendList)
      nullify(forcing % snowFlux % recvList)
      nullify(forcing % snowFlux % copyList)
      forcing % snowFlux % ioinfo % input = .false.
      forcing % snowFlux % ioinfo % sfc = .false.
      forcing % snowFlux % ioinfo % restart = .false.
      forcing % snowFlux % ioinfo % output = .true.
      forcing % snowFlux % block => b

      allocate(forcing % iceFraction)
      allocate(forcing % iceFraction % ioinfo)
      forcing % iceFraction % fieldName = 'iceFraction'
      forcing % iceFraction % isVarArray = .false.
      forcing % iceFraction % isPersistent = .true.
      forcing % iceFraction % isActive = .true.
      allocate(forcing % iceFraction % array(nCells + 1))
      forcing % iceFraction % array = 0.0_RKIND
      forcing % iceFraction % dimSizes(1) = nCells
      forcing % iceFraction % dimNames(1) = 'nCells'
      forcing % iceFraction % hasTimeDimension = .true.
      nullify(forcing % iceFraction % prev)
      nullify(forcing % iceFraction % next)
      nullify(forcing % iceFraction % sendList)
      nullify(forcing % iceFraction % recvList)
      nullify(forcing % iceFraction % copyList)
      forcing % iceFraction % ioinfo % input = .false.
      forcing % iceFraction % ioinfo % sfc = .false.
      forcing % iceFraction % ioinfo % restart = .false.
      forcing % iceFraction % ioinfo % output = .true.
      forcing % iceFraction % block => b

      allocate(forcing % prognosticCO2)
      allocate(forcing % prognosticCO2 % ioinfo)
      forcing % prognosticCO2 % fieldName = 'prognosticCO2'
      forcing % prognosticCO2 % isVarArray = .false.
      forcing % prognosticCO2 % isPersistent = .true.
      forcing % prognosticCO2 % isActive = .true.
      allocate(forcing % prognosticCO2 % array(nCells + 1))
      forcing % prognosticCO2 % array = 0.0_RKIND
      forcing % prognosticCO2 % dimSizes(1) = nCells
      forcing % prognosticCO2 % dimNames(1) = 'nCells'
      forcing % prognosticCO2 % hasTimeDimension = .true.
      nullify(forcing % prognosticCO2 % prev)
      nullify(forcing % prognosticCO2 % next)
      nullify(forcing % prognosticCO2 % sendList)
      nullify(forcing % prognosticCO2 % recvList)
      nullify(forcing % prognosticCO2 % copyList)
      forcing % prognosticCO2 % ioinfo % input = .false.
      forcing % prognosticCO2 % ioinfo % sfc = .false.
      forcing % prognosticCO2 % ioinfo % restart = .false.
      forcing % prognosticCO2 % ioinfo % output = .true.
      forcing % prognosticCO2 % block => b

      allocate(forcing % diagnosticCO2)
      allocate(forcing % diagnosticCO2 % ioinfo)
      forcing % diagnosticCO2 % fieldName = 'diagnosticCO2'
      forcing % diagnosticCO2 % isVarArray = .false.
      forcing % diagnosticCO2 % isPersistent = .true.
      forcing % diagnosticCO2 % isActive = .true.
      allocate(forcing % diagnosticCO2 % array(nCells + 1))
      forcing % diagnosticCO2 % array = 0.0_RKIND
      forcing % diagnosticCO2 % dimSizes(1) = nCells
      forcing % diagnosticCO2 % dimNames(1) = 'nCells'
      forcing % diagnosticCO2 % hasTimeDimension = .true.
      nullify(forcing % diagnosticCO2 % prev)
      nullify(forcing % diagnosticCO2 % next)
      nullify(forcing % diagnosticCO2 % sendList)
      nullify(forcing % diagnosticCO2 % recvList)
      nullify(forcing % diagnosticCO2 % copyList)
      forcing % diagnosticCO2 % ioinfo % input = .false.
      forcing % diagnosticCO2 % ioinfo % sfc = .false.
      forcing % diagnosticCO2 % ioinfo % restart = .false.
      forcing % diagnosticCO2 % ioinfo % output = .true.
      forcing % diagnosticCO2 % block => b

      allocate(forcing % squaredWindSpeed10Meter)
      allocate(forcing % squaredWindSpeed10Meter % ioinfo)
      forcing % squaredWindSpeed10Meter % fieldName = 'squaredWindSpeed10Meter'
      forcing % squaredWindSpeed10Meter % isVarArray = .false.
      forcing % squaredWindSpeed10Meter % isPersistent = .true.
      forcing % squaredWindSpeed10Meter % isActive = .true.
      allocate(forcing % squaredWindSpeed10Meter % array(nCells + 1))
      forcing % squaredWindSpeed10Meter % array = 0.0_RKIND
      forcing % squaredWindSpeed10Meter % dimSizes(1) = nCells
      forcing % squaredWindSpeed10Meter % dimNames(1) = 'nCells'
      forcing % squaredWindSpeed10Meter % hasTimeDimension = .true.
      nullify(forcing % squaredWindSpeed10Meter % prev)
      nullify(forcing % squaredWindSpeed10Meter % next)
      nullify(forcing % squaredWindSpeed10Meter % sendList)
      nullify(forcing % squaredWindSpeed10Meter % recvList)
      nullify(forcing % squaredWindSpeed10Meter % copyList)
      forcing % squaredWindSpeed10Meter % ioinfo % input = .false.
      forcing % squaredWindSpeed10Meter % ioinfo % sfc = .false.
      forcing % squaredWindSpeed10Meter % ioinfo % restart = .false.
      forcing % squaredWindSpeed10Meter % ioinfo % output = .true.
      forcing % squaredWindSpeed10Meter % block => b

      allocate(forcing % nAccumulatedCoupled)
      allocate(forcing % nAccumulatedCoupled % ioinfo)
      forcing % nAccumulatedCoupled % fieldName = 'nAccumulatedCoupled'
      forcing % nAccumulatedCoupled % isVarArray = .false.
      forcing % nAccumulatedCoupled % hasTimeDimension = .true.
      nullify(forcing % nAccumulatedCoupled % prev)
      nullify(forcing % nAccumulatedCoupled % next)
      nullify(forcing % nAccumulatedCoupled % sendList)
      nullify(forcing % nAccumulatedCoupled % recvList)
      nullify(forcing % nAccumulatedCoupled % copyList)
      forcing % nAccumulatedCoupled % ioinfo % input = .false.
      forcing % nAccumulatedCoupled % ioinfo % sfc = .false.
      forcing % nAccumulatedCoupled % ioinfo % restart = .false.
      forcing % nAccumulatedCoupled % ioinfo % output = .true.
      forcing % nAccumulatedCoupled % block => b

      allocate(forcing % CO2Flux)
      allocate(forcing % CO2Flux % ioinfo)
      forcing % CO2Flux % fieldName = 'CO2Flux'
      forcing % CO2Flux % isVarArray = .false.
      forcing % CO2Flux % isPersistent = .true.
      forcing % CO2Flux % isActive = .true.
      allocate(forcing % CO2Flux % array(nCells + 1))
      forcing % CO2Flux % array = 0.0_RKIND
      forcing % CO2Flux % dimSizes(1) = nCells
      forcing % CO2Flux % dimNames(1) = 'nCells'
      forcing % CO2Flux % hasTimeDimension = .true.
      nullify(forcing % CO2Flux % prev)
      nullify(forcing % CO2Flux % next)
      nullify(forcing % CO2Flux % sendList)
      nullify(forcing % CO2Flux % recvList)
      nullify(forcing % CO2Flux % copyList)
      forcing % CO2Flux % ioinfo % input = .false.
      forcing % CO2Flux % ioinfo % sfc = .false.
      forcing % CO2Flux % ioinfo % restart = .false.
      forcing % CO2Flux % ioinfo % output = .true.
      forcing % CO2Flux % block => b

      allocate(forcing % DMSFlux)
      allocate(forcing % DMSFlux % ioinfo)
      forcing % DMSFlux % fieldName = 'DMSFlux'
      forcing % DMSFlux % isVarArray = .false.
      forcing % DMSFlux % isPersistent = .true.
      forcing % DMSFlux % isActive = .true.
      allocate(forcing % DMSFlux % array(nCells + 1))
      forcing % DMSFlux % array = 0.0_RKIND
      forcing % DMSFlux % dimSizes(1) = nCells
      forcing % DMSFlux % dimNames(1) = 'nCells'
      forcing % DMSFlux % hasTimeDimension = .true.
      nullify(forcing % DMSFlux % prev)
      nullify(forcing % DMSFlux % next)
      nullify(forcing % DMSFlux % sendList)
      nullify(forcing % DMSFlux % recvList)
      nullify(forcing % DMSFlux % copyList)
      forcing % DMSFlux % ioinfo % input = .false.
      forcing % DMSFlux % ioinfo % sfc = .false.
      forcing % DMSFlux % ioinfo % restart = .false.
      forcing % DMSFlux % ioinfo % output = .true.
      forcing % DMSFlux % block => b

   end subroutine mpas_allocate_forcing


   subroutine mpas_allocate_scratch(b, scratch, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                         )

      implicit none

      type (block_type), pointer :: b
      type (scratch_type), intent(inout) :: scratch
      logical :: group_started
      integer :: index_counter
      integer :: group_counter
      integer :: group_start
      integer, intent(in) :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      scratch % block => b
      allocate(scratch % vorticityGradientTangentialComponent)
      allocate(scratch % vorticityGradientTangentialComponent % ioinfo)
      scratch % vorticityGradientTangentialComponent % fieldName = 'vorticityGradientTangentialComponent'
      scratch % vorticityGradientTangentialComponent % isVarArray = .false.
      scratch % vorticityGradientTangentialComponent % isPersistent = .false.
      scratch % vorticityGradientTangentialComponent % isActive = .false.
      nullify(scratch % vorticityGradientTangentialComponent % array)
      scratch % vorticityGradientTangentialComponent % dimSizes(1) = nVertLevels
      scratch % vorticityGradientTangentialComponent % dimNames(1) = 'nVertLevels'
      scratch % vorticityGradientTangentialComponent % dimSizes(2) = nEdges+1
      scratch % vorticityGradientTangentialComponent % dimNames(2) = 'nEdges'
      scratch % vorticityGradientTangentialComponent % hasTimeDimension = .true.
      nullify(scratch % vorticityGradientTangentialComponent % prev)
      nullify(scratch % vorticityGradientTangentialComponent % next)
      nullify(scratch % vorticityGradientTangentialComponent % sendList)
      nullify(scratch % vorticityGradientTangentialComponent % recvList)
      nullify(scratch % vorticityGradientTangentialComponent % copyList)
      scratch % vorticityGradientTangentialComponent % ioinfo % input = .false.
      scratch % vorticityGradientTangentialComponent % ioinfo % sfc = .false.
      scratch % vorticityGradientTangentialComponent % ioinfo % restart = .false.
      scratch % vorticityGradientTangentialComponent % ioinfo % output = .false.
      scratch % vorticityGradientTangentialComponent % block => b

      allocate(scratch % vorticityGradientNormalComponent)
      allocate(scratch % vorticityGradientNormalComponent % ioinfo)
      scratch % vorticityGradientNormalComponent % fieldName = 'vorticityGradientNormalComponent'
      scratch % vorticityGradientNormalComponent % isVarArray = .false.
      scratch % vorticityGradientNormalComponent % isPersistent = .false.
      scratch % vorticityGradientNormalComponent % isActive = .false.
      nullify(scratch % vorticityGradientNormalComponent % array)
      scratch % vorticityGradientNormalComponent % dimSizes(1) = nVertLevels
      scratch % vorticityGradientNormalComponent % dimNames(1) = 'nVertLevels'
      scratch % vorticityGradientNormalComponent % dimSizes(2) = nEdges+1
      scratch % vorticityGradientNormalComponent % dimNames(2) = 'nEdges'
      scratch % vorticityGradientNormalComponent % hasTimeDimension = .true.
      nullify(scratch % vorticityGradientNormalComponent % prev)
      nullify(scratch % vorticityGradientNormalComponent % next)
      nullify(scratch % vorticityGradientNormalComponent % sendList)
      nullify(scratch % vorticityGradientNormalComponent % recvList)
      nullify(scratch % vorticityGradientNormalComponent % copyList)
      scratch % vorticityGradientNormalComponent % ioinfo % input = .false.
      scratch % vorticityGradientNormalComponent % ioinfo % sfc = .false.
      scratch % vorticityGradientNormalComponent % ioinfo % restart = .false.
      scratch % vorticityGradientNormalComponent % ioinfo % output = .false.
      scratch % vorticityGradientNormalComponent % block => b

      allocate(scratch % normalizedRelativeVorticityVertex)
      allocate(scratch % normalizedRelativeVorticityVertex % ioinfo)
      scratch % normalizedRelativeVorticityVertex % fieldName = 'normalizedRelativeVorticityVertex'
      scratch % normalizedRelativeVorticityVertex % isVarArray = .false.
      scratch % normalizedRelativeVorticityVertex % isPersistent = .false.
      scratch % normalizedRelativeVorticityVertex % isActive = .false.
      nullify(scratch % normalizedRelativeVorticityVertex % array)
      scratch % normalizedRelativeVorticityVertex % dimSizes(1) = nVertLevels
      scratch % normalizedRelativeVorticityVertex % dimNames(1) = 'nVertLevels'
      scratch % normalizedRelativeVorticityVertex % dimSizes(2) = nVertices+1
      scratch % normalizedRelativeVorticityVertex % dimNames(2) = 'nVertices'
      scratch % normalizedRelativeVorticityVertex % hasTimeDimension = .true.
      nullify(scratch % normalizedRelativeVorticityVertex % prev)
      nullify(scratch % normalizedRelativeVorticityVertex % next)
      nullify(scratch % normalizedRelativeVorticityVertex % sendList)
      nullify(scratch % normalizedRelativeVorticityVertex % recvList)
      nullify(scratch % normalizedRelativeVorticityVertex % copyList)
      scratch % normalizedRelativeVorticityVertex % ioinfo % input = .false.
      scratch % normalizedRelativeVorticityVertex % ioinfo % sfc = .false.
      scratch % normalizedRelativeVorticityVertex % ioinfo % restart = .false.
      scratch % normalizedRelativeVorticityVertex % ioinfo % output = .false.
      scratch % normalizedRelativeVorticityVertex % block => b

      allocate(scratch % normalizedPlanetaryVorticityVertex)
      allocate(scratch % normalizedPlanetaryVorticityVertex % ioinfo)
      scratch % normalizedPlanetaryVorticityVertex % fieldName = 'normalizedPlanetaryVorticityVertex'
      scratch % normalizedPlanetaryVorticityVertex % isVarArray = .false.
      scratch % normalizedPlanetaryVorticityVertex % isPersistent = .false.
      scratch % normalizedPlanetaryVorticityVertex % isActive = .false.
      nullify(scratch % normalizedPlanetaryVorticityVertex % array)
      scratch % normalizedPlanetaryVorticityVertex % dimSizes(1) = nVertLevels
      scratch % normalizedPlanetaryVorticityVertex % dimNames(1) = 'nVertLevels'
      scratch % normalizedPlanetaryVorticityVertex % dimSizes(2) = nVertices+1
      scratch % normalizedPlanetaryVorticityVertex % dimNames(2) = 'nVertices'
      scratch % normalizedPlanetaryVorticityVertex % hasTimeDimension = .true.
      nullify(scratch % normalizedPlanetaryVorticityVertex % prev)
      nullify(scratch % normalizedPlanetaryVorticityVertex % next)
      nullify(scratch % normalizedPlanetaryVorticityVertex % sendList)
      nullify(scratch % normalizedPlanetaryVorticityVertex % recvList)
      nullify(scratch % normalizedPlanetaryVorticityVertex % copyList)
      scratch % normalizedPlanetaryVorticityVertex % ioinfo % input = .false.
      scratch % normalizedPlanetaryVorticityVertex % ioinfo % sfc = .false.
      scratch % normalizedPlanetaryVorticityVertex % ioinfo % restart = .false.
      scratch % normalizedPlanetaryVorticityVertex % ioinfo % output = .false.
      scratch % normalizedPlanetaryVorticityVertex % block => b

      allocate(scratch % kineticEnergyVertex)
      allocate(scratch % kineticEnergyVertex % ioinfo)
      scratch % kineticEnergyVertex % fieldName = 'kineticEnergyVertex'
      scratch % kineticEnergyVertex % isVarArray = .false.
      scratch % kineticEnergyVertex % isPersistent = .false.
      scratch % kineticEnergyVertex % isActive = .false.
      nullify(scratch % kineticEnergyVertex % array)
      scratch % kineticEnergyVertex % dimSizes(1) = nVertLevels
      scratch % kineticEnergyVertex % dimNames(1) = 'nVertLevels'
      scratch % kineticEnergyVertex % dimSizes(2) = nVertices+1
      scratch % kineticEnergyVertex % dimNames(2) = 'nVertices'
      scratch % kineticEnergyVertex % hasTimeDimension = .true.
      nullify(scratch % kineticEnergyVertex % prev)
      nullify(scratch % kineticEnergyVertex % next)
      nullify(scratch % kineticEnergyVertex % sendList)
      nullify(scratch % kineticEnergyVertex % recvList)
      nullify(scratch % kineticEnergyVertex % copyList)
      scratch % kineticEnergyVertex % ioinfo % input = .false.
      scratch % kineticEnergyVertex % ioinfo % sfc = .false.
      scratch % kineticEnergyVertex % ioinfo % restart = .false.
      scratch % kineticEnergyVertex % ioinfo % output = .false.
      scratch % kineticEnergyVertex % block => b

      allocate(scratch % kineticEnergyVertexOnCells)
      allocate(scratch % kineticEnergyVertexOnCells % ioinfo)
      scratch % kineticEnergyVertexOnCells % fieldName = 'kineticEnergyVertexOnCells'
      scratch % kineticEnergyVertexOnCells % isVarArray = .false.
      scratch % kineticEnergyVertexOnCells % isPersistent = .false.
      scratch % kineticEnergyVertexOnCells % isActive = .false.
      nullify(scratch % kineticEnergyVertexOnCells % array)
      scratch % kineticEnergyVertexOnCells % dimSizes(1) = nVertLevels
      scratch % kineticEnergyVertexOnCells % dimNames(1) = 'nVertLevels'
      scratch % kineticEnergyVertexOnCells % dimSizes(2) = nCells+1
      scratch % kineticEnergyVertexOnCells % dimNames(2) = 'nCells'
      scratch % kineticEnergyVertexOnCells % hasTimeDimension = .true.
      nullify(scratch % kineticEnergyVertexOnCells % prev)
      nullify(scratch % kineticEnergyVertexOnCells % next)
      nullify(scratch % kineticEnergyVertexOnCells % sendList)
      nullify(scratch % kineticEnergyVertexOnCells % recvList)
      nullify(scratch % kineticEnergyVertexOnCells % copyList)
      scratch % kineticEnergyVertexOnCells % ioinfo % input = .false.
      scratch % kineticEnergyVertexOnCells % ioinfo % sfc = .false.
      scratch % kineticEnergyVertexOnCells % ioinfo % restart = .false.
      scratch % kineticEnergyVertexOnCells % ioinfo % output = .false.
      scratch % kineticEnergyVertexOnCells % block => b

      allocate(scratch % densitySurfaceDisplaced)
      allocate(scratch % densitySurfaceDisplaced % ioinfo)
      scratch % densitySurfaceDisplaced % fieldName = 'densitySurfaceDisplaced'
      scratch % densitySurfaceDisplaced % isVarArray = .false.
      scratch % densitySurfaceDisplaced % isPersistent = .false.
      scratch % densitySurfaceDisplaced % isActive = .false.
      nullify(scratch % densitySurfaceDisplaced % array)
      scratch % densitySurfaceDisplaced % dimSizes(1) = nVertLevels
      scratch % densitySurfaceDisplaced % dimNames(1) = 'nVertLevels'
      scratch % densitySurfaceDisplaced % dimSizes(2) = nCells+1
      scratch % densitySurfaceDisplaced % dimNames(2) = 'nCells'
      scratch % densitySurfaceDisplaced % hasTimeDimension = .true.
      nullify(scratch % densitySurfaceDisplaced % prev)
      nullify(scratch % densitySurfaceDisplaced % next)
      nullify(scratch % densitySurfaceDisplaced % sendList)
      nullify(scratch % densitySurfaceDisplaced % recvList)
      nullify(scratch % densitySurfaceDisplaced % copyList)
      scratch % densitySurfaceDisplaced % ioinfo % input = .false.
      scratch % densitySurfaceDisplaced % ioinfo % sfc = .false.
      scratch % densitySurfaceDisplaced % ioinfo % restart = .false.
      scratch % densitySurfaceDisplaced % ioinfo % output = .false.
      scratch % densitySurfaceDisplaced % block => b

      allocate(scratch % thermalExpansionCoeff)
      allocate(scratch % thermalExpansionCoeff % ioinfo)
      scratch % thermalExpansionCoeff % fieldName = 'thermalExpansionCoeff'
      scratch % thermalExpansionCoeff % isVarArray = .false.
      scratch % thermalExpansionCoeff % isPersistent = .true.
      scratch % thermalExpansionCoeff % isActive = .true.
      allocate(scratch % thermalExpansionCoeff % array(nVertLevels, nCells + 1))
      scratch % thermalExpansionCoeff % array = 0.0_RKIND
      scratch % thermalExpansionCoeff % dimSizes(1) = nVertLevels
      scratch % thermalExpansionCoeff % dimNames(1) = 'nVertLevels'
      scratch % thermalExpansionCoeff % dimSizes(2) = nCells
      scratch % thermalExpansionCoeff % dimNames(2) = 'nCells'
      scratch % thermalExpansionCoeff % hasTimeDimension = .true.
      nullify(scratch % thermalExpansionCoeff % prev)
      nullify(scratch % thermalExpansionCoeff % next)
      nullify(scratch % thermalExpansionCoeff % sendList)
      nullify(scratch % thermalExpansionCoeff % recvList)
      nullify(scratch % thermalExpansionCoeff % copyList)
      scratch % thermalExpansionCoeff % ioinfo % input = .false.
      scratch % thermalExpansionCoeff % ioinfo % sfc = .false.
      scratch % thermalExpansionCoeff % ioinfo % restart = .false.
      scratch % thermalExpansionCoeff % ioinfo % output = .true.
      scratch % thermalExpansionCoeff % block => b

      allocate(scratch % salineContractionCoeff)
      allocate(scratch % salineContractionCoeff % ioinfo)
      scratch % salineContractionCoeff % fieldName = 'salineContractionCoeff'
      scratch % salineContractionCoeff % isVarArray = .false.
      scratch % salineContractionCoeff % isPersistent = .true.
      scratch % salineContractionCoeff % isActive = .true.
      allocate(scratch % salineContractionCoeff % array(nVertLevels, nCells + 1))
      scratch % salineContractionCoeff % array = 0.0_RKIND
      scratch % salineContractionCoeff % dimSizes(1) = nVertLevels
      scratch % salineContractionCoeff % dimNames(1) = 'nVertLevels'
      scratch % salineContractionCoeff % dimSizes(2) = nCells
      scratch % salineContractionCoeff % dimNames(2) = 'nCells'
      scratch % salineContractionCoeff % hasTimeDimension = .true.
      nullify(scratch % salineContractionCoeff % prev)
      nullify(scratch % salineContractionCoeff % next)
      nullify(scratch % salineContractionCoeff % sendList)
      nullify(scratch % salineContractionCoeff % recvList)
      nullify(scratch % salineContractionCoeff % copyList)
      scratch % salineContractionCoeff % ioinfo % input = .false.
      scratch % salineContractionCoeff % ioinfo % sfc = .false.
      scratch % salineContractionCoeff % ioinfo % restart = .false.
      scratch % salineContractionCoeff % ioinfo % output = .true.
      scratch % salineContractionCoeff % block => b

      allocate(scratch % normalVelocityTest)
      allocate(scratch % normalVelocityTest % ioinfo)
      scratch % normalVelocityTest % fieldName = 'normalVelocityTest'
      scratch % normalVelocityTest % isVarArray = .false.
      scratch % normalVelocityTest % isPersistent = .false.
      scratch % normalVelocityTest % isActive = .false.
      nullify(scratch % normalVelocityTest % array)
      scratch % normalVelocityTest % dimSizes(1) = nVertLevels
      scratch % normalVelocityTest % dimNames(1) = 'nVertLevels'
      scratch % normalVelocityTest % dimSizes(2) = nEdges+1
      scratch % normalVelocityTest % dimNames(2) = 'nEdges'
      scratch % normalVelocityTest % hasTimeDimension = .false.
      nullify(scratch % normalVelocityTest % prev)
      nullify(scratch % normalVelocityTest % next)
      nullify(scratch % normalVelocityTest % sendList)
      nullify(scratch % normalVelocityTest % recvList)
      nullify(scratch % normalVelocityTest % copyList)
      scratch % normalVelocityTest % ioinfo % input = .false.
      scratch % normalVelocityTest % ioinfo % sfc = .false.
      scratch % normalVelocityTest % ioinfo % restart = .false.
      scratch % normalVelocityTest % ioinfo % output = .false.
      scratch % normalVelocityTest % block => b

      allocate(scratch % tangentialVelocityTest)
      allocate(scratch % tangentialVelocityTest % ioinfo)
      scratch % tangentialVelocityTest % fieldName = 'tangentialVelocityTest'
      scratch % tangentialVelocityTest % isVarArray = .false.
      scratch % tangentialVelocityTest % isPersistent = .false.
      scratch % tangentialVelocityTest % isActive = .false.
      nullify(scratch % tangentialVelocityTest % array)
      scratch % tangentialVelocityTest % dimSizes(1) = nVertLevels
      scratch % tangentialVelocityTest % dimNames(1) = 'nVertLevels'
      scratch % tangentialVelocityTest % dimSizes(2) = nEdges+1
      scratch % tangentialVelocityTest % dimNames(2) = 'nEdges'
      scratch % tangentialVelocityTest % hasTimeDimension = .false.
      nullify(scratch % tangentialVelocityTest % prev)
      nullify(scratch % tangentialVelocityTest % next)
      nullify(scratch % tangentialVelocityTest % sendList)
      nullify(scratch % tangentialVelocityTest % recvList)
      nullify(scratch % tangentialVelocityTest % copyList)
      scratch % tangentialVelocityTest % ioinfo % input = .false.
      scratch % tangentialVelocityTest % ioinfo % sfc = .false.
      scratch % tangentialVelocityTest % ioinfo % restart = .false.
      scratch % tangentialVelocityTest % ioinfo % output = .false.
      scratch % tangentialVelocityTest % block => b

      allocate(scratch % strainRateR3Cell)
      allocate(scratch % strainRateR3Cell % ioinfo)
      scratch % strainRateR3Cell % fieldName = 'strainRateR3Cell'
      scratch % strainRateR3Cell % isVarArray = .false.
      scratch % strainRateR3Cell % isPersistent = .false.
      scratch % strainRateR3Cell % isActive = .false.
      nullify(scratch % strainRateR3Cell % array)
      scratch % strainRateR3Cell % dimSizes(1) = 6
      scratch % strainRateR3Cell % dimNames(1) = 'SIX'
      scratch % strainRateR3Cell % dimSizes(2) = nVertLevels
      scratch % strainRateR3Cell % dimNames(2) = 'nVertLevels'
      scratch % strainRateR3Cell % dimSizes(3) = nCells+1
      scratch % strainRateR3Cell % dimNames(3) = 'nCells'
      scratch % strainRateR3Cell % hasTimeDimension = .false.
      nullify(scratch % strainRateR3Cell % prev)
      nullify(scratch % strainRateR3Cell % next)
      nullify(scratch % strainRateR3Cell % sendList)
      nullify(scratch % strainRateR3Cell % recvList)
      nullify(scratch % strainRateR3Cell % copyList)
      scratch % strainRateR3Cell % ioinfo % input = .false.
      scratch % strainRateR3Cell % ioinfo % sfc = .false.
      scratch % strainRateR3Cell % ioinfo % restart = .false.
      scratch % strainRateR3Cell % ioinfo % output = .false.
      scratch % strainRateR3Cell % block => b

      allocate(scratch % strainRateR3CellSolution)
      allocate(scratch % strainRateR3CellSolution % ioinfo)
      scratch % strainRateR3CellSolution % fieldName = 'strainRateR3CellSolution'
      scratch % strainRateR3CellSolution % isVarArray = .false.
      scratch % strainRateR3CellSolution % isPersistent = .false.
      scratch % strainRateR3CellSolution % isActive = .false.
      nullify(scratch % strainRateR3CellSolution % array)
      scratch % strainRateR3CellSolution % dimSizes(1) = 6
      scratch % strainRateR3CellSolution % dimNames(1) = 'SIX'
      scratch % strainRateR3CellSolution % dimSizes(2) = nVertLevels
      scratch % strainRateR3CellSolution % dimNames(2) = 'nVertLevels'
      scratch % strainRateR3CellSolution % dimSizes(3) = nCells+1
      scratch % strainRateR3CellSolution % dimNames(3) = 'nCells'
      scratch % strainRateR3CellSolution % hasTimeDimension = .false.
      nullify(scratch % strainRateR3CellSolution % prev)
      nullify(scratch % strainRateR3CellSolution % next)
      nullify(scratch % strainRateR3CellSolution % sendList)
      nullify(scratch % strainRateR3CellSolution % recvList)
      nullify(scratch % strainRateR3CellSolution % copyList)
      scratch % strainRateR3CellSolution % ioinfo % input = .false.
      scratch % strainRateR3CellSolution % ioinfo % sfc = .false.
      scratch % strainRateR3CellSolution % ioinfo % restart = .false.
      scratch % strainRateR3CellSolution % ioinfo % output = .false.
      scratch % strainRateR3CellSolution % block => b

      allocate(scratch % strainRateR3Edge)
      allocate(scratch % strainRateR3Edge % ioinfo)
      scratch % strainRateR3Edge % fieldName = 'strainRateR3Edge'
      scratch % strainRateR3Edge % isVarArray = .false.
      scratch % strainRateR3Edge % isPersistent = .false.
      scratch % strainRateR3Edge % isActive = .false.
      nullify(scratch % strainRateR3Edge % array)
      scratch % strainRateR3Edge % dimSizes(1) = 6
      scratch % strainRateR3Edge % dimNames(1) = 'SIX'
      scratch % strainRateR3Edge % dimSizes(2) = nVertLevels
      scratch % strainRateR3Edge % dimNames(2) = 'nVertLevels'
      scratch % strainRateR3Edge % dimSizes(3) = nEdges+1
      scratch % strainRateR3Edge % dimNames(3) = 'nEdges'
      scratch % strainRateR3Edge % hasTimeDimension = .false.
      nullify(scratch % strainRateR3Edge % prev)
      nullify(scratch % strainRateR3Edge % next)
      nullify(scratch % strainRateR3Edge % sendList)
      nullify(scratch % strainRateR3Edge % recvList)
      nullify(scratch % strainRateR3Edge % copyList)
      scratch % strainRateR3Edge % ioinfo % input = .false.
      scratch % strainRateR3Edge % ioinfo % sfc = .false.
      scratch % strainRateR3Edge % ioinfo % restart = .false.
      scratch % strainRateR3Edge % ioinfo % output = .false.
      scratch % strainRateR3Edge % block => b

      allocate(scratch % strainRateLonLatRCell)
      allocate(scratch % strainRateLonLatRCell % ioinfo)
      scratch % strainRateLonLatRCell % fieldName = 'strainRateLonLatRCell'
      scratch % strainRateLonLatRCell % isVarArray = .false.
      scratch % strainRateLonLatRCell % isPersistent = .false.
      scratch % strainRateLonLatRCell % isActive = .false.
      nullify(scratch % strainRateLonLatRCell % array)
      scratch % strainRateLonLatRCell % dimSizes(1) = 6
      scratch % strainRateLonLatRCell % dimNames(1) = 'SIX'
      scratch % strainRateLonLatRCell % dimSizes(2) = nVertLevels
      scratch % strainRateLonLatRCell % dimNames(2) = 'nVertLevels'
      scratch % strainRateLonLatRCell % dimSizes(3) = nCells+1
      scratch % strainRateLonLatRCell % dimNames(3) = 'nCells'
      scratch % strainRateLonLatRCell % hasTimeDimension = .false.
      nullify(scratch % strainRateLonLatRCell % prev)
      nullify(scratch % strainRateLonLatRCell % next)
      nullify(scratch % strainRateLonLatRCell % sendList)
      nullify(scratch % strainRateLonLatRCell % recvList)
      nullify(scratch % strainRateLonLatRCell % copyList)
      scratch % strainRateLonLatRCell % ioinfo % input = .false.
      scratch % strainRateLonLatRCell % ioinfo % sfc = .false.
      scratch % strainRateLonLatRCell % ioinfo % restart = .false.
      scratch % strainRateLonLatRCell % ioinfo % output = .false.
      scratch % strainRateLonLatRCell % block => b

      allocate(scratch % strainRateLonLatRCellSolution)
      allocate(scratch % strainRateLonLatRCellSolution % ioinfo)
      scratch % strainRateLonLatRCellSolution % fieldName = 'strainRateLonLatRCellSolution'
      scratch % strainRateLonLatRCellSolution % isVarArray = .false.
      scratch % strainRateLonLatRCellSolution % isPersistent = .false.
      scratch % strainRateLonLatRCellSolution % isActive = .false.
      nullify(scratch % strainRateLonLatRCellSolution % array)
      scratch % strainRateLonLatRCellSolution % dimSizes(1) = 6
      scratch % strainRateLonLatRCellSolution % dimNames(1) = 'SIX'
      scratch % strainRateLonLatRCellSolution % dimSizes(2) = nVertLevels
      scratch % strainRateLonLatRCellSolution % dimNames(2) = 'nVertLevels'
      scratch % strainRateLonLatRCellSolution % dimSizes(3) = nCells+1
      scratch % strainRateLonLatRCellSolution % dimNames(3) = 'nCells'
      scratch % strainRateLonLatRCellSolution % hasTimeDimension = .false.
      nullify(scratch % strainRateLonLatRCellSolution % prev)
      nullify(scratch % strainRateLonLatRCellSolution % next)
      nullify(scratch % strainRateLonLatRCellSolution % sendList)
      nullify(scratch % strainRateLonLatRCellSolution % recvList)
      nullify(scratch % strainRateLonLatRCellSolution % copyList)
      scratch % strainRateLonLatRCellSolution % ioinfo % input = .false.
      scratch % strainRateLonLatRCellSolution % ioinfo % sfc = .false.
      scratch % strainRateLonLatRCellSolution % ioinfo % restart = .false.
      scratch % strainRateLonLatRCellSolution % ioinfo % output = .false.
      scratch % strainRateLonLatRCellSolution % block => b

      allocate(scratch % strainRateLonLatREdge)
      allocate(scratch % strainRateLonLatREdge % ioinfo)
      scratch % strainRateLonLatREdge % fieldName = 'strainRateLonLatREdge'
      scratch % strainRateLonLatREdge % isVarArray = .false.
      scratch % strainRateLonLatREdge % isPersistent = .false.
      scratch % strainRateLonLatREdge % isActive = .false.
      nullify(scratch % strainRateLonLatREdge % array)
      scratch % strainRateLonLatREdge % dimSizes(1) = 6
      scratch % strainRateLonLatREdge % dimNames(1) = 'SIX'
      scratch % strainRateLonLatREdge % dimSizes(2) = nVertLevels
      scratch % strainRateLonLatREdge % dimNames(2) = 'nVertLevels'
      scratch % strainRateLonLatREdge % dimSizes(3) = nEdges+1
      scratch % strainRateLonLatREdge % dimNames(3) = 'nEdges'
      scratch % strainRateLonLatREdge % hasTimeDimension = .false.
      nullify(scratch % strainRateLonLatREdge % prev)
      nullify(scratch % strainRateLonLatREdge % next)
      nullify(scratch % strainRateLonLatREdge % sendList)
      nullify(scratch % strainRateLonLatREdge % recvList)
      nullify(scratch % strainRateLonLatREdge % copyList)
      scratch % strainRateLonLatREdge % ioinfo % input = .false.
      scratch % strainRateLonLatREdge % ioinfo % sfc = .false.
      scratch % strainRateLonLatREdge % ioinfo % restart = .false.
      scratch % strainRateLonLatREdge % ioinfo % output = .false.
      scratch % strainRateLonLatREdge % block => b

      allocate(scratch % divTensorR3Cell)
      allocate(scratch % divTensorR3Cell % ioinfo)
      scratch % divTensorR3Cell % fieldName = 'divTensorR3Cell'
      scratch % divTensorR3Cell % isVarArray = .false.
      scratch % divTensorR3Cell % isPersistent = .false.
      scratch % divTensorR3Cell % isActive = .false.
      nullify(scratch % divTensorR3Cell % array)
      scratch % divTensorR3Cell % dimSizes(1) = 3
      scratch % divTensorR3Cell % dimNames(1) = 'R3'
      scratch % divTensorR3Cell % dimSizes(2) = nVertLevels
      scratch % divTensorR3Cell % dimNames(2) = 'nVertLevels'
      scratch % divTensorR3Cell % dimSizes(3) = nCells+1
      scratch % divTensorR3Cell % dimNames(3) = 'nCells'
      scratch % divTensorR3Cell % hasTimeDimension = .false.
      nullify(scratch % divTensorR3Cell % prev)
      nullify(scratch % divTensorR3Cell % next)
      nullify(scratch % divTensorR3Cell % sendList)
      nullify(scratch % divTensorR3Cell % recvList)
      nullify(scratch % divTensorR3Cell % copyList)
      scratch % divTensorR3Cell % ioinfo % input = .false.
      scratch % divTensorR3Cell % ioinfo % sfc = .false.
      scratch % divTensorR3Cell % ioinfo % restart = .false.
      scratch % divTensorR3Cell % ioinfo % output = .false.
      scratch % divTensorR3Cell % block => b

      allocate(scratch % divTensorR3CellSolution)
      allocate(scratch % divTensorR3CellSolution % ioinfo)
      scratch % divTensorR3CellSolution % fieldName = 'divTensorR3CellSolution'
      scratch % divTensorR3CellSolution % isVarArray = .false.
      scratch % divTensorR3CellSolution % isPersistent = .false.
      scratch % divTensorR3CellSolution % isActive = .false.
      nullify(scratch % divTensorR3CellSolution % array)
      scratch % divTensorR3CellSolution % dimSizes(1) = 3
      scratch % divTensorR3CellSolution % dimNames(1) = 'R3'
      scratch % divTensorR3CellSolution % dimSizes(2) = nVertLevels
      scratch % divTensorR3CellSolution % dimNames(2) = 'nVertLevels'
      scratch % divTensorR3CellSolution % dimSizes(3) = nCells+1
      scratch % divTensorR3CellSolution % dimNames(3) = 'nCells'
      scratch % divTensorR3CellSolution % hasTimeDimension = .false.
      nullify(scratch % divTensorR3CellSolution % prev)
      nullify(scratch % divTensorR3CellSolution % next)
      nullify(scratch % divTensorR3CellSolution % sendList)
      nullify(scratch % divTensorR3CellSolution % recvList)
      nullify(scratch % divTensorR3CellSolution % copyList)
      scratch % divTensorR3CellSolution % ioinfo % input = .false.
      scratch % divTensorR3CellSolution % ioinfo % sfc = .false.
      scratch % divTensorR3CellSolution % ioinfo % restart = .false.
      scratch % divTensorR3CellSolution % ioinfo % output = .false.
      scratch % divTensorR3CellSolution % block => b

      allocate(scratch % divTensorLonLatRCell)
      allocate(scratch % divTensorLonLatRCell % ioinfo)
      scratch % divTensorLonLatRCell % fieldName = 'divTensorLonLatRCell'
      scratch % divTensorLonLatRCell % isVarArray = .false.
      scratch % divTensorLonLatRCell % isPersistent = .false.
      scratch % divTensorLonLatRCell % isActive = .false.
      nullify(scratch % divTensorLonLatRCell % array)
      scratch % divTensorLonLatRCell % dimSizes(1) = 3
      scratch % divTensorLonLatRCell % dimNames(1) = 'R3'
      scratch % divTensorLonLatRCell % dimSizes(2) = nVertLevels
      scratch % divTensorLonLatRCell % dimNames(2) = 'nVertLevels'
      scratch % divTensorLonLatRCell % dimSizes(3) = nCells+1
      scratch % divTensorLonLatRCell % dimNames(3) = 'nCells'
      scratch % divTensorLonLatRCell % hasTimeDimension = .false.
      nullify(scratch % divTensorLonLatRCell % prev)
      nullify(scratch % divTensorLonLatRCell % next)
      nullify(scratch % divTensorLonLatRCell % sendList)
      nullify(scratch % divTensorLonLatRCell % recvList)
      nullify(scratch % divTensorLonLatRCell % copyList)
      scratch % divTensorLonLatRCell % ioinfo % input = .false.
      scratch % divTensorLonLatRCell % ioinfo % sfc = .false.
      scratch % divTensorLonLatRCell % ioinfo % restart = .false.
      scratch % divTensorLonLatRCell % ioinfo % output = .false.
      scratch % divTensorLonLatRCell % block => b

      allocate(scratch % divTensorLonLatRCellSolution)
      allocate(scratch % divTensorLonLatRCellSolution % ioinfo)
      scratch % divTensorLonLatRCellSolution % fieldName = 'divTensorLonLatRCellSolution'
      scratch % divTensorLonLatRCellSolution % isVarArray = .false.
      scratch % divTensorLonLatRCellSolution % isPersistent = .false.
      scratch % divTensorLonLatRCellSolution % isActive = .false.
      nullify(scratch % divTensorLonLatRCellSolution % array)
      scratch % divTensorLonLatRCellSolution % dimSizes(1) = 3
      scratch % divTensorLonLatRCellSolution % dimNames(1) = 'R3'
      scratch % divTensorLonLatRCellSolution % dimSizes(2) = nVertLevels
      scratch % divTensorLonLatRCellSolution % dimNames(2) = 'nVertLevels'
      scratch % divTensorLonLatRCellSolution % dimSizes(3) = nCells+1
      scratch % divTensorLonLatRCellSolution % dimNames(3) = 'nCells'
      scratch % divTensorLonLatRCellSolution % hasTimeDimension = .false.
      nullify(scratch % divTensorLonLatRCellSolution % prev)
      nullify(scratch % divTensorLonLatRCellSolution % next)
      nullify(scratch % divTensorLonLatRCellSolution % sendList)
      nullify(scratch % divTensorLonLatRCellSolution % recvList)
      nullify(scratch % divTensorLonLatRCellSolution % copyList)
      scratch % divTensorLonLatRCellSolution % ioinfo % input = .false.
      scratch % divTensorLonLatRCellSolution % ioinfo % sfc = .false.
      scratch % divTensorLonLatRCellSolution % ioinfo % restart = .false.
      scratch % divTensorLonLatRCellSolution % ioinfo % output = .false.
      scratch % divTensorLonLatRCellSolution % block => b

      allocate(scratch % outerProductEdge)
      allocate(scratch % outerProductEdge % ioinfo)
      scratch % outerProductEdge % fieldName = 'outerProductEdge'
      scratch % outerProductEdge % isVarArray = .false.
      scratch % outerProductEdge % isPersistent = .false.
      scratch % outerProductEdge % isActive = .false.
      nullify(scratch % outerProductEdge % array)
      scratch % outerProductEdge % dimSizes(1) = 6
      scratch % outerProductEdge % dimNames(1) = 'SIX'
      scratch % outerProductEdge % dimSizes(2) = nVertLevels
      scratch % outerProductEdge % dimNames(2) = 'nVertLevels'
      scratch % outerProductEdge % dimSizes(3) = nEdges+1
      scratch % outerProductEdge % dimNames(3) = 'nEdges'
      scratch % outerProductEdge % hasTimeDimension = .false.
      nullify(scratch % outerProductEdge % prev)
      nullify(scratch % outerProductEdge % next)
      nullify(scratch % outerProductEdge % sendList)
      nullify(scratch % outerProductEdge % recvList)
      nullify(scratch % outerProductEdge % copyList)
      scratch % outerProductEdge % ioinfo % input = .false.
      scratch % outerProductEdge % ioinfo % sfc = .false.
      scratch % outerProductEdge % ioinfo % restart = .false.
      scratch % outerProductEdge % ioinfo % output = .false.
      scratch % outerProductEdge % block => b

      allocate(scratch % normalVectorEdge)
      allocate(scratch % normalVectorEdge % ioinfo)
      scratch % normalVectorEdge % fieldName = 'normalVectorEdge'
      scratch % normalVectorEdge % isVarArray = .false.
      scratch % normalVectorEdge % isPersistent = .false.
      scratch % normalVectorEdge % isActive = .false.
      nullify(scratch % normalVectorEdge % array)
      scratch % normalVectorEdge % dimSizes(1) = nVertLevels
      scratch % normalVectorEdge % dimNames(1) = 'nVertLevels'
      scratch % normalVectorEdge % dimSizes(2) = nEdges+1
      scratch % normalVectorEdge % dimNames(2) = 'nEdges'
      scratch % normalVectorEdge % hasTimeDimension = .false.
      nullify(scratch % normalVectorEdge % prev)
      nullify(scratch % normalVectorEdge % next)
      nullify(scratch % normalVectorEdge % sendList)
      nullify(scratch % normalVectorEdge % recvList)
      nullify(scratch % normalVectorEdge % copyList)
      scratch % normalVectorEdge % ioinfo % input = .false.
      scratch % normalVectorEdge % ioinfo % sfc = .false.
      scratch % normalVectorEdge % ioinfo % restart = .false.
      scratch % normalVectorEdge % ioinfo % output = .false.
      scratch % normalVectorEdge % block => b

      allocate(scratch % tangentialVectorEdge)
      allocate(scratch % tangentialVectorEdge % ioinfo)
      scratch % tangentialVectorEdge % fieldName = 'tangentialVectorEdge'
      scratch % tangentialVectorEdge % isVarArray = .false.
      scratch % tangentialVectorEdge % isPersistent = .false.
      scratch % tangentialVectorEdge % isActive = .false.
      nullify(scratch % tangentialVectorEdge % array)
      scratch % tangentialVectorEdge % dimSizes(1) = nVertLevels
      scratch % tangentialVectorEdge % dimNames(1) = 'nVertLevels'
      scratch % tangentialVectorEdge % dimSizes(2) = nEdges+1
      scratch % tangentialVectorEdge % dimNames(2) = 'nEdges'
      scratch % tangentialVectorEdge % hasTimeDimension = .false.
      nullify(scratch % tangentialVectorEdge % prev)
      nullify(scratch % tangentialVectorEdge % next)
      nullify(scratch % tangentialVectorEdge % sendList)
      nullify(scratch % tangentialVectorEdge % recvList)
      nullify(scratch % tangentialVectorEdge % copyList)
      scratch % tangentialVectorEdge % ioinfo % input = .false.
      scratch % tangentialVectorEdge % ioinfo % sfc = .false.
      scratch % tangentialVectorEdge % ioinfo % restart = .false.
      scratch % tangentialVectorEdge % ioinfo % output = .false.
      scratch % tangentialVectorEdge % block => b

      allocate(scratch % windStressFull)
      allocate(scratch % windStressFull % ioinfo)
      scratch % windStressFull % fieldName = 'windStressFull'
      scratch % windStressFull % isVarArray = .false.
      scratch % windStressFull % isPersistent = .false.
      scratch % windStressFull % isActive = .false.
      nullify(scratch % windStressFull % array)
      scratch % windStressFull % dimSizes(1) = nVertLevels
      scratch % windStressFull % dimNames(1) = 'nVertLevels'
      scratch % windStressFull % dimSizes(2) = nEdges+1
      scratch % windStressFull % dimNames(2) = 'nEdges'
      scratch % windStressFull % hasTimeDimension = .true.
      nullify(scratch % windStressFull % prev)
      nullify(scratch % windStressFull % next)
      nullify(scratch % windStressFull % sendList)
      nullify(scratch % windStressFull % recvList)
      nullify(scratch % windStressFull % copyList)
      scratch % windStressFull % ioinfo % input = .false.
      scratch % windStressFull % ioinfo % sfc = .false.
      scratch % windStressFull % ioinfo % restart = .false.
      scratch % windStressFull % ioinfo % output = .false.
      scratch % windStressFull % block => b

      allocate(scratch % windStressX)
      allocate(scratch % windStressX % ioinfo)
      scratch % windStressX % fieldName = 'windStressX'
      scratch % windStressX % isVarArray = .false.
      scratch % windStressX % isPersistent = .false.
      scratch % windStressX % isActive = .false.
      nullify(scratch % windStressX % array)
      scratch % windStressX % dimSizes(1) = nVertLevels
      scratch % windStressX % dimNames(1) = 'nVertLevels'
      scratch % windStressX % dimSizes(2) = nCells+1
      scratch % windStressX % dimNames(2) = 'nCells'
      scratch % windStressX % hasTimeDimension = .true.
      nullify(scratch % windStressX % prev)
      nullify(scratch % windStressX % next)
      nullify(scratch % windStressX % sendList)
      nullify(scratch % windStressX % recvList)
      nullify(scratch % windStressX % copyList)
      scratch % windStressX % ioinfo % input = .false.
      scratch % windStressX % ioinfo % sfc = .false.
      scratch % windStressX % ioinfo % restart = .false.
      scratch % windStressX % ioinfo % output = .false.
      scratch % windStressX % block => b

      allocate(scratch % windStressY)
      allocate(scratch % windStressY % ioinfo)
      scratch % windStressY % fieldName = 'windStressY'
      scratch % windStressY % isVarArray = .false.
      scratch % windStressY % isPersistent = .false.
      scratch % windStressY % isActive = .false.
      nullify(scratch % windStressY % array)
      scratch % windStressY % dimSizes(1) = nVertLevels
      scratch % windStressY % dimNames(1) = 'nVertLevels'
      scratch % windStressY % dimSizes(2) = nCells+1
      scratch % windStressY % dimNames(2) = 'nCells'
      scratch % windStressY % hasTimeDimension = .true.
      nullify(scratch % windStressY % prev)
      nullify(scratch % windStressY % next)
      nullify(scratch % windStressY % sendList)
      nullify(scratch % windStressY % recvList)
      nullify(scratch % windStressY % copyList)
      scratch % windStressY % ioinfo % input = .false.
      scratch % windStressY % ioinfo % sfc = .false.
      scratch % windStressY % ioinfo % restart = .false.
      scratch % windStressY % ioinfo % output = .false.
      scratch % windStressY % block => b

      allocate(scratch % windStressZ)
      allocate(scratch % windStressZ % ioinfo)
      scratch % windStressZ % fieldName = 'windStressZ'
      scratch % windStressZ % isVarArray = .false.
      scratch % windStressZ % isPersistent = .false.
      scratch % windStressZ % isActive = .false.
      nullify(scratch % windStressZ % array)
      scratch % windStressZ % dimSizes(1) = nVertLevels
      scratch % windStressZ % dimNames(1) = 'nVertLevels'
      scratch % windStressZ % dimSizes(2) = nCells+1
      scratch % windStressZ % dimNames(2) = 'nCells'
      scratch % windStressZ % hasTimeDimension = .true.
      nullify(scratch % windStressZ % prev)
      nullify(scratch % windStressZ % next)
      nullify(scratch % windStressZ % sendList)
      nullify(scratch % windStressZ % recvList)
      nullify(scratch % windStressZ % copyList)
      scratch % windStressZ % ioinfo % input = .false.
      scratch % windStressZ % ioinfo % sfc = .false.
      scratch % windStressZ % ioinfo % restart = .false.
      scratch % windStressZ % ioinfo % output = .false.
      scratch % windStressZ % block => b

      allocate(scratch % windStressZonal)
      allocate(scratch % windStressZonal % ioinfo)
      scratch % windStressZonal % fieldName = 'windStressZonal'
      scratch % windStressZonal % isVarArray = .false.
      scratch % windStressZonal % isPersistent = .false.
      scratch % windStressZonal % isActive = .false.
      nullify(scratch % windStressZonal % array)
      scratch % windStressZonal % dimSizes(1) = nVertLevels
      scratch % windStressZonal % dimNames(1) = 'nVertLevels'
      scratch % windStressZonal % dimSizes(2) = nCells+1
      scratch % windStressZonal % dimNames(2) = 'nCells'
      scratch % windStressZonal % hasTimeDimension = .true.
      nullify(scratch % windStressZonal % prev)
      nullify(scratch % windStressZonal % next)
      nullify(scratch % windStressZonal % sendList)
      nullify(scratch % windStressZonal % recvList)
      nullify(scratch % windStressZonal % copyList)
      scratch % windStressZonal % ioinfo % input = .false.
      scratch % windStressZonal % ioinfo % sfc = .false.
      scratch % windStressZonal % ioinfo % restart = .false.
      scratch % windStressZonal % ioinfo % output = .false.
      scratch % windStressZonal % block => b

      allocate(scratch % windStressMeridional)
      allocate(scratch % windStressMeridional % ioinfo)
      scratch % windStressMeridional % fieldName = 'windStressMeridional'
      scratch % windStressMeridional % isVarArray = .false.
      scratch % windStressMeridional % isPersistent = .false.
      scratch % windStressMeridional % isActive = .false.
      nullify(scratch % windStressMeridional % array)
      scratch % windStressMeridional % dimSizes(1) = nVertLevels
      scratch % windStressMeridional % dimNames(1) = 'nVertLevels'
      scratch % windStressMeridional % dimSizes(2) = nCells+1
      scratch % windStressMeridional % dimNames(2) = 'nCells'
      scratch % windStressMeridional % hasTimeDimension = .true.
      nullify(scratch % windStressMeridional % prev)
      nullify(scratch % windStressMeridional % next)
      nullify(scratch % windStressMeridional % sendList)
      nullify(scratch % windStressMeridional % recvList)
      nullify(scratch % windStressMeridional % copyList)
      scratch % windStressMeridional % ioinfo % input = .false.
      scratch % windStressMeridional % ioinfo % sfc = .false.
      scratch % windStressMeridional % ioinfo % restart = .false.
      scratch % windStressMeridional % ioinfo % output = .false.
      scratch % windStressMeridional % block => b

   end subroutine mpas_allocate_scratch



   subroutine mpas_setup_provis_state(b)!{{{
      type (block_type), pointer :: b
      type (block_type), pointer :: block

      integer :: nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels

      block => b
      do while(associated(block))
      nCells = block % mesh % nCells
      nEdges = block % mesh % nEdges
      maxEdges = block % mesh % maxEdges
      maxEdges2 = block % mesh % maxEdges2
      nVertices = block % mesh % nVertices
      vertexDegree = block % mesh % vertexDegree
      nVertLevels = block % mesh % nVertLevels


         allocate(block % provis_state)
         call mpas_allocate_state(block, block % provis_state, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                              )

         block => block % next 
      end do

      block => b
      do while(associated(block))
         if(associated(block % prev) .and. associated(block % next)) then
            call mpas_create_state_links(block % provis_state, prev = block % prev % provis_state, next = block % next % &
provis_state)
         else if(associated(block % prev)) then
            call mpas_create_state_links(block % provis_state, prev = block % prev % provis_state)
         else if(associated(block % next)) then
            call mpas_create_state_links(block % provis_state, next = block % next % provis_state)
         else
            call mpas_create_state_links(block % provis_state)
         end if
         block => block % next 
      end do
   end subroutine mpas_setup_provis_state!}}}

   subroutine mpas_deallocate_provis_state(b)!{{{
      type (block_type), pointer :: b
      type (block_type), pointer :: block

      block => b
      do while(associated(block))
         call mpas_deallocate_state(block % provis_state)
         deallocate(block % provis_state)
         block => block % next
      end do
   end subroutine mpas_deallocate_provis_state!}}}

!***********************************************************************
!
!  routine mpas_deallocate_domain
!
!> \brief   MPAS Domain deallocation routine
!> \author  Michael Duda
!> \date    04/02/13
!> \details 
!> This routine deallocates a domain structure. 
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_domain(dom)!{{{

      implicit none

      type (domain_type), pointer :: dom !< Input/Output: Domain to deallocate

      type (block_type), pointer :: block_ptr

      block_ptr => dom % blocklist
      do while (associated(block_ptr))
         call mpas_deallocate_block(block_ptr)
         block_ptr => block_ptr % next
      end do

      deallocate(dom) 

   end subroutine mpas_deallocate_domain!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field1d_integer
!
!> \brief   MPAS 1D Scratch integer allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 1D scratch integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field1d_integer(f, single_block_in)!{{{
       type (field1dInteger), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated or all blocks.
       logical :: single_block
       type (field1dInteger), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field1d_integer!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field2d_integer
!
!> \brief   MPAS 2D Scratch integer allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 2D scratch integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field2d_integer(f, single_block_in)!{{{
       type (field2dInteger), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field2dInteger), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1), f_cursor % dimSizes(2)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1), f % dimSizes(2)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field2d_integer!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field3d_integer
!
!> \brief   MPAS 3D Scratch integer allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 3D scratch integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field3d_integer(f, single_block_in)!{{{
       type (field3dInteger), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field3dInteger), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1), f_cursor % dimSizes(2), f_cursor % dimSizes(3)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1), f % dimSizes(2), f % dimSizes(3)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field3d_integer!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field1d_real
!
!> \brief   MPAS 1D Scratch real allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 1D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field1d_real(f, single_block_in)!{{{
       type (field1dReal), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field1dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field1d_real!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field2d_real
!
!> \brief   MPAS 2D Scratch real allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 2D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field2d_real(f, single_block_in)!{{{
       type (field2dReal), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field2dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1), f_cursor % dimSizes(2)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1), f % dimSizes(2)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field2d_real!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field3d_real
!
!> \brief   MPAS 3D Scratch real allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 3D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field3d_real(f, single_block_in)!{{{
       type (field3dReal), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field3dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1), f_cursor % dimSizes(2), f_cursor % dimSizes(3)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1), f % dimSizes(2), f % dimSizes(3)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field3d_real!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field4D_real
!
!> \brief   MPAS 4D Scratch real allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 4D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field4d_real(f, single_block_in)!{{{
       type (field4dReal), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field4dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1), f_cursor % dimSizes(2), f_cursor % dimSizes(3), f_cursor % dimSizes(4)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1), f % dimSizes(2), f % dimSizes(3), f % dimSizes(4)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field4d_real!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field5D_real
!
!> \brief   MPAS 5D Scratch real allocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 5D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field5d_real(f, single_block_in)!{{{
       type (field5dReal), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field5dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1), f_cursor % dimSizes(2), f_cursor % dimSizes(3), f_cursor % dimSizes(4), f_cursor % dimSizes(5)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1), f % dimSizes(2), f % dimSizes(3), f % dimSizes(4), f % dimSizes(5)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field5d_real!}}}

!***********************************************************************
!
!  routine mpas_allocate_scratch_field1D_char
!
!> \brief   MPAS 1D Scratch character deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine allocates a 1D scratch character field.
!
!-----------------------------------------------------------------------
   subroutine mpas_allocate_scratch_field1d_char(f, single_block_in)!{{{
       type (field1dChar), pointer :: f !< Input: Field to allocate
       logical, intent(in), optional :: single_block_in !< Input: Logical flag that determines if a single block should be allocated, or all blocks.
       logical :: single_block
       type (field1dChar), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not. single_block) then
          f_cursor => f
          do while(associated(f_cursor))
             if(.not.associated(f_cursor % array)) then
                allocate(f_cursor % array(f_cursor % dimSizes(1)))
             end if
             f_cursor => f_cursor % next
          end do
       else
          if(.not.associated(f % array)) then
            allocate(f % array(f % dimSizes(1)))
          end if
       end if

   end subroutine mpas_allocate_scratch_field1d_char!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field1D_integer
!
!> \brief   MPAS 1D Scratch integer deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 1D scratch integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field1d_integer(f, single_block_in)!{{{
       type (field1dInteger), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field1dInteger), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field1d_integer!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field2D_integer
!
!> \brief   MPAS 2D Scratch integer deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 2D scratch integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field2d_integer(f, single_block_in)!{{{
       type (field2dInteger), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field2dInteger), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field2d_integer!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field3D_integer
!
!> \brief   MPAS 3D Scratch integer deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 3D scratch integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field3d_integer(f, single_block_in)!{{{
       type (field3dInteger), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field3dInteger), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field3d_integer!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field1D_real
!
!> \brief   MPAS 1D Scratch real deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 1D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field1d_real(f, single_block_in)!{{{
       type (field1dReal), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field1dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field1d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field2D_real
!
!> \brief   MPAS 2D Scratch real deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 2D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field2d_real(f, single_block_in)!{{{
       type (field2dReal), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field2dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field2d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field3D_real
!
!> \brief   MPAS 3D Scratch real deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 3D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field3d_real(f, single_block_in)!{{{
       type (field3dReal), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field3dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field3d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field4D_real
!
!> \brief   MPAS 4D Scratch real deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 4D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field4d_real(f, single_block_in)!{{{
       type (field4dReal), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field4dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field4d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field5D_real
!
!> \brief   MPAS 5D Scratch real deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 5D scratch real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field5d_real(f, single_block_in)!{{{
       type (field5dReal), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field5dReal), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field5d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_scratch_field1D_char
!
!> \brief   MPAS 1D Scratch character deallocation rotuine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 1D scratch character field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_scratch_field1d_char(f, single_block_in)!{{{
       type (field1dChar), pointer :: f !< Input: Field to deallocate
       logical, intent(in), optional :: single_block_in !< Input: Logical that determines if a single block should be deallocated, or all blocks.
       logical :: single_block
       type (field1dChar), pointer :: f_cursor

       if(f % isPersistent) then
          return
       end if

       if(present(single_block_in)) then
          single_block = single_block_in
       else
          single_block = .false.
       end if

       if(.not.single_block) then
          f_cursor => f
          do while(associated(f_cursor))
            if(associated(f_cursor % array)) then
              deallocate(f_cursor % array)
            end if
   
            f_cursor => f_cursor % next
          end do
       else
          if(associated(f % array)) then
             deallocate(f % array)
          end if
       end if

   end subroutine mpas_deallocate_scratch_field1d_char!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field0d_integer
!
!> \brief   MPAS 0D integer deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 0D integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field0d_integer(f)!{{{
       type (field0dInteger), pointer :: f !< Input: Field to deallocate
       type (field0dInteger), pointer :: f_cursor

       f_cursor => f

       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         deallocate(f_cursor)
         f_cursor => f
       end do

   end subroutine mpas_deallocate_field0d_integer!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field1D_integer
!
!> \brief   MPAS 1D integer deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 1D integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field1d_integer(f)!{{{
       type (field1dInteger), pointer :: f !< Input: Field to deallocate
       type (field1dInteger), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field1d_integer!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field2D_integer
!
!> \brief   MPAS 2D integer deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 2D integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field2d_integer(f)!{{{
       type (field2dInteger), pointer :: f !< Input: Field to deallocate
       type (field2dInteger), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field2d_integer!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field3D_integer
!
!> \brief   MPAS 3D integer deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 3D integer field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field3d_integer(f)!{{{
       type (field3dInteger), pointer :: f !< Input: Field to deallocate
       type (field3dInteger), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field3d_integer!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field0d_real
!
!> \brief   MPAS 0D real deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 0D real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field0d_real(f)!{{{
       type (field0dReal), pointer :: f !< Input: Field to deallocate
       type (field0dReal), pointer :: f_cursor

       f_cursor => f

       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field0d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field1D_real
!
!> \brief   MPAS 1D real deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 1D real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field1d_real(f)!{{{
       type (field1dReal), pointer :: f !< Input: Field to deallocate
       type (field1dReal), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field1d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field2D_real
!
!> \brief   MPAS 2D real deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 2D real field.
!

   subroutine mpas_deallocate_field2d_real(f)!{{{
       type (field2dReal), pointer :: f !< Input: Field to deallocate
       type (field2dReal), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field2d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field3D_real
!
!> \brief   MPAS 3D real deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 3D real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field3d_real(f)!{{{
       type (field3dReal), pointer :: f !< Input: Field to deallocate
       type (field3dReal), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field3d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field4D_real
!
!> \brief   MPAS 4D real deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 4D real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field4d_real(f)!{{{
       type (field4dReal), pointer :: f !< Input: Field to deallocate
       type (field4dReal), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field4d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field5D_real
!
!> \brief   MPAS 5D real deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 5D real field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field5d_real(f)!{{{
       type (field5dReal), pointer :: f !< Input: Field to deallocate
       type (field5dReal), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field5d_real!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field0D_char
!
!> \brief   MPAS 0D character deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 0D character field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field0d_char(f)!{{{
       type (field0dChar), pointer :: f !< Input: Field to deallocate
       type (field0dChar), pointer :: f_cursor

       f_cursor => f

       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         deallocate(f_cursor)
         f_cursor => f
       end do

   end subroutine mpas_deallocate_field0d_char!}}}

!***********************************************************************
!
!  routine mpas_deallocate_field1D_char
!
!> \brief   MPAS 1D character deallocation routine.
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a 1D character field.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_field1d_char(f)!{{{
       type (field1dChar), pointer :: f !< Input: Field to deallocate
       type (field1dChar), pointer :: f_cursor

       f_cursor => f
       do while(associated(f_cursor))
         if(associated(f % next)) then
           f => f % next
         else
           nullify(f)
         end if

         if(associated(f_cursor % ioinfo)) then
           deallocate(f_cursor % ioinfo)
         end if

         if(associated(f_cursor % array)) then
           deallocate(f_cursor % array)
         end if

         deallocate(f_cursor)

         f_cursor => f
       end do

   end subroutine mpas_deallocate_field1d_char!}}}

!***********************************************************************
!
!  routine mpas_deallocate_block
!
!> \brief   MPAS Block deallocation routine
!> \author  Doug Jacobsen
!> \date    04/02/13
!> \details 
!> This routine deallocates a block structure.
!
!-----------------------------------------------------------------------
   subroutine mpas_deallocate_block(b)!{{{
 
      implicit none

      type (block_type), intent(inout) :: b !< Input/Output: Block to be deallocated.

      integer :: i

      ! BUG: It seems like we should be deallocating the exchange lists before we 
      !      deallocate the array of head pointers and the parinfo type...
      !      It also seems like these deallocations should happen with mpas_dmpar_destroy_multihalo_exchange_list

      deallocate(b % parinfo % cellsToSend)
      deallocate(b % parinfo % cellsToRecv)
      deallocate(b % parinfo % cellsToCopy)

      deallocate(b % parinfo % edgesToSend)
      deallocate(b % parinfo % edgesToRecv)
      deallocate(b % parinfo % edgesToCopy)

      deallocate(b % parinfo % verticesToSend)
      deallocate(b % parinfo % verticesToRecv)
      deallocate(b % parinfo % verticesToCopy)

      deallocate(b % parinfo)

      do i=1,b % state % nTimeLevels
         call mpas_deallocate_state(b % state % time_levs(i) % state)
         deallocate(b % state % time_levs(i) % state)
      end do
      deallocate(b % state % time_levs)
      deallocate(b % state)

      call mpas_deallocate_mesh(b % mesh)
      deallocate(b % mesh)

      call mpas_deallocate_verticalMesh(b % verticalMesh)
      deallocate(b % verticalMesh)

      call mpas_deallocate_tend(b % tend)
      deallocate(b % tend)

      call mpas_deallocate_diagnostics(b % diagnostics)
      deallocate(b % diagnostics)

      call mpas_deallocate_average(b % average)
      deallocate(b % average)

      call mpas_deallocate_forcing(b % forcing)
      deallocate(b % forcing)

      call mpas_deallocate_scratch(b % scratch)
      deallocate(b % scratch)


   end subroutine mpas_deallocate_block!}}}


   subroutine mpas_deallocate_state(state)

      implicit none

      type (state_type), intent(inout) :: state

      if(associated(state % tracers % array)) then
         deallocate(state % tracers % array)
      end if
      deallocate(state % tracers % ioinfo)
      call mpas_deallocate_attlist(state % tracers % attList)
      deallocate(state % tracers)

      if(associated(state % normalVelocity % array)) then
         deallocate(state % normalVelocity % array)
      end if
      deallocate(state % normalVelocity % ioinfo)
      call mpas_deallocate_attlist(state % normalVelocity % attList)
      deallocate(state % normalVelocity)

      if(associated(state % layerThickness % array)) then
         deallocate(state % layerThickness % array)
      end if
      deallocate(state % layerThickness % ioinfo)
      call mpas_deallocate_attlist(state % layerThickness % attList)
      deallocate(state % layerThickness)

      if(associated(state % ssh % array)) then
         deallocate(state % ssh % array)
      end if
      deallocate(state % ssh % ioinfo)
      call mpas_deallocate_attlist(state % ssh % attList)
      deallocate(state % ssh)

      if(associated(state % highFreqThickness % array)) then
         deallocate(state % highFreqThickness % array)
      end if
      deallocate(state % highFreqThickness % ioinfo)
      call mpas_deallocate_attlist(state % highFreqThickness % attList)
      deallocate(state % highFreqThickness)

      if(associated(state % lowFreqDivergence % array)) then
         deallocate(state % lowFreqDivergence % array)
      end if
      deallocate(state % lowFreqDivergence % ioinfo)
      call mpas_deallocate_attlist(state % lowFreqDivergence % attList)
      deallocate(state % lowFreqDivergence)

      if(associated(state % normalBarotropicVelocity % array)) then
         deallocate(state % normalBarotropicVelocity % array)
      end if
      deallocate(state % normalBarotropicVelocity % ioinfo)
      call mpas_deallocate_attlist(state % normalBarotropicVelocity % attList)
      deallocate(state % normalBarotropicVelocity)

      if(associated(state % normalBarotropicVelocitySubcycle % array)) then
         deallocate(state % normalBarotropicVelocitySubcycle % array)
      end if
      deallocate(state % normalBarotropicVelocitySubcycle % ioinfo)
      call mpas_deallocate_attlist(state % normalBarotropicVelocitySubcycle % attList)
      deallocate(state % normalBarotropicVelocitySubcycle)

      if(associated(state % sshSubcycle % array)) then
         deallocate(state % sshSubcycle % array)
      end if
      deallocate(state % sshSubcycle % ioinfo)
      call mpas_deallocate_attlist(state % sshSubcycle % attList)
      deallocate(state % sshSubcycle)

      if(associated(state % normalBaroclinicVelocity % array)) then
         deallocate(state % normalBaroclinicVelocity % array)
      end if
      deallocate(state % normalBaroclinicVelocity % ioinfo)
      call mpas_deallocate_attlist(state % normalBaroclinicVelocity % attList)
      deallocate(state % normalBaroclinicVelocity)

   end subroutine mpas_deallocate_state


   subroutine mpas_deallocate_mesh(mesh)

      implicit none

      type (mesh_type), intent(inout) :: mesh

      if(associated(mesh % latCell % array)) then
         deallocate(mesh % latCell % array)
      end if
      deallocate(mesh % latCell % ioinfo)
      call mpas_deallocate_attlist(mesh % latCell % attList)
      deallocate(mesh % latCell)

      if(associated(mesh % lonCell % array)) then
         deallocate(mesh % lonCell % array)
      end if
      deallocate(mesh % lonCell % ioinfo)
      call mpas_deallocate_attlist(mesh % lonCell % attList)
      deallocate(mesh % lonCell)

      if(associated(mesh % xCell % array)) then
         deallocate(mesh % xCell % array)
      end if
      deallocate(mesh % xCell % ioinfo)
      call mpas_deallocate_attlist(mesh % xCell % attList)
      deallocate(mesh % xCell)

      if(associated(mesh % yCell % array)) then
         deallocate(mesh % yCell % array)
      end if
      deallocate(mesh % yCell % ioinfo)
      call mpas_deallocate_attlist(mesh % yCell % attList)
      deallocate(mesh % yCell)

      if(associated(mesh % zCell % array)) then
         deallocate(mesh % zCell % array)
      end if
      deallocate(mesh % zCell % ioinfo)
      call mpas_deallocate_attlist(mesh % zCell % attList)
      deallocate(mesh % zCell)

      if(associated(mesh % indexToCellID % array)) then
         deallocate(mesh % indexToCellID % array)
      end if
      deallocate(mesh % indexToCellID % ioinfo)
      call mpas_deallocate_attlist(mesh % indexToCellID % attList)
      deallocate(mesh % indexToCellID)

      if(associated(mesh % latEdge % array)) then
         deallocate(mesh % latEdge % array)
      end if
      deallocate(mesh % latEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % latEdge % attList)
      deallocate(mesh % latEdge)

      if(associated(mesh % lonEdge % array)) then
         deallocate(mesh % lonEdge % array)
      end if
      deallocate(mesh % lonEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % lonEdge % attList)
      deallocate(mesh % lonEdge)

      if(associated(mesh % xEdge % array)) then
         deallocate(mesh % xEdge % array)
      end if
      deallocate(mesh % xEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % xEdge % attList)
      deallocate(mesh % xEdge)

      if(associated(mesh % yEdge % array)) then
         deallocate(mesh % yEdge % array)
      end if
      deallocate(mesh % yEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % yEdge % attList)
      deallocate(mesh % yEdge)

      if(associated(mesh % zEdge % array)) then
         deallocate(mesh % zEdge % array)
      end if
      deallocate(mesh % zEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % zEdge % attList)
      deallocate(mesh % zEdge)

      if(associated(mesh % indexToEdgeID % array)) then
         deallocate(mesh % indexToEdgeID % array)
      end if
      deallocate(mesh % indexToEdgeID % ioinfo)
      call mpas_deallocate_attlist(mesh % indexToEdgeID % attList)
      deallocate(mesh % indexToEdgeID)

      if(associated(mesh % latVertex % array)) then
         deallocate(mesh % latVertex % array)
      end if
      deallocate(mesh % latVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % latVertex % attList)
      deallocate(mesh % latVertex)

      if(associated(mesh % lonVertex % array)) then
         deallocate(mesh % lonVertex % array)
      end if
      deallocate(mesh % lonVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % lonVertex % attList)
      deallocate(mesh % lonVertex)

      if(associated(mesh % xVertex % array)) then
         deallocate(mesh % xVertex % array)
      end if
      deallocate(mesh % xVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % xVertex % attList)
      deallocate(mesh % xVertex)

      if(associated(mesh % yVertex % array)) then
         deallocate(mesh % yVertex % array)
      end if
      deallocate(mesh % yVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % yVertex % attList)
      deallocate(mesh % yVertex)

      if(associated(mesh % zVertex % array)) then
         deallocate(mesh % zVertex % array)
      end if
      deallocate(mesh % zVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % zVertex % attList)
      deallocate(mesh % zVertex)

      if(associated(mesh % indexToVertexID % array)) then
         deallocate(mesh % indexToVertexID % array)
      end if
      deallocate(mesh % indexToVertexID % ioinfo)
      call mpas_deallocate_attlist(mesh % indexToVertexID % attList)
      deallocate(mesh % indexToVertexID)

      if(associated(mesh % meshDensity % array)) then
         deallocate(mesh % meshDensity % array)
      end if
      deallocate(mesh % meshDensity % ioinfo)
      call mpas_deallocate_attlist(mesh % meshDensity % attList)
      deallocate(mesh % meshDensity)

      if(associated(mesh % meshScalingDel2 % array)) then
         deallocate(mesh % meshScalingDel2 % array)
      end if
      deallocate(mesh % meshScalingDel2 % ioinfo)
      call mpas_deallocate_attlist(mesh % meshScalingDel2 % attList)
      deallocate(mesh % meshScalingDel2)

      if(associated(mesh % meshScalingDel4 % array)) then
         deallocate(mesh % meshScalingDel4 % array)
      end if
      deallocate(mesh % meshScalingDel4 % ioinfo)
      call mpas_deallocate_attlist(mesh % meshScalingDel4 % attList)
      deallocate(mesh % meshScalingDel4)

      if(associated(mesh % meshScaling % array)) then
         deallocate(mesh % meshScaling % array)
      end if
      deallocate(mesh % meshScaling % ioinfo)
      call mpas_deallocate_attlist(mesh % meshScaling % attList)
      deallocate(mesh % meshScaling)

      if(associated(mesh % cellsOnEdge % array)) then
         deallocate(mesh % cellsOnEdge % array)
      end if
      deallocate(mesh % cellsOnEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % cellsOnEdge % attList)
      deallocate(mesh % cellsOnEdge)

      if(associated(mesh % nEdgesOnCell % array)) then
         deallocate(mesh % nEdgesOnCell % array)
      end if
      deallocate(mesh % nEdgesOnCell % ioinfo)
      call mpas_deallocate_attlist(mesh % nEdgesOnCell % attList)
      deallocate(mesh % nEdgesOnCell)

      if(associated(mesh % nEdgesOnEdge % array)) then
         deallocate(mesh % nEdgesOnEdge % array)
      end if
      deallocate(mesh % nEdgesOnEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % nEdgesOnEdge % attList)
      deallocate(mesh % nEdgesOnEdge)

      if(associated(mesh % edgesOnCell % array)) then
         deallocate(mesh % edgesOnCell % array)
      end if
      deallocate(mesh % edgesOnCell % ioinfo)
      call mpas_deallocate_attlist(mesh % edgesOnCell % attList)
      deallocate(mesh % edgesOnCell)

      if(associated(mesh % edgesOnEdge % array)) then
         deallocate(mesh % edgesOnEdge % array)
      end if
      deallocate(mesh % edgesOnEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % edgesOnEdge % attList)
      deallocate(mesh % edgesOnEdge)

      if(associated(mesh % weightsOnEdge % array)) then
         deallocate(mesh % weightsOnEdge % array)
      end if
      deallocate(mesh % weightsOnEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % weightsOnEdge % attList)
      deallocate(mesh % weightsOnEdge)

      if(associated(mesh % dvEdge % array)) then
         deallocate(mesh % dvEdge % array)
      end if
      deallocate(mesh % dvEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % dvEdge % attList)
      deallocate(mesh % dvEdge)

      if(associated(mesh % dcEdge % array)) then
         deallocate(mesh % dcEdge % array)
      end if
      deallocate(mesh % dcEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % dcEdge % attList)
      deallocate(mesh % dcEdge)

      if(associated(mesh % angleEdge % array)) then
         deallocate(mesh % angleEdge % array)
      end if
      deallocate(mesh % angleEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % angleEdge % attList)
      deallocate(mesh % angleEdge)

      if(associated(mesh % areaCell % array)) then
         deallocate(mesh % areaCell % array)
      end if
      deallocate(mesh % areaCell % ioinfo)
      call mpas_deallocate_attlist(mesh % areaCell % attList)
      deallocate(mesh % areaCell)

      if(associated(mesh % areaTriangle % array)) then
         deallocate(mesh % areaTriangle % array)
      end if
      deallocate(mesh % areaTriangle % ioinfo)
      call mpas_deallocate_attlist(mesh % areaTriangle % attList)
      deallocate(mesh % areaTriangle)

      if(associated(mesh % edgeNormalVectors % array)) then
         deallocate(mesh % edgeNormalVectors % array)
      end if
      deallocate(mesh % edgeNormalVectors % ioinfo)
      call mpas_deallocate_attlist(mesh % edgeNormalVectors % attList)
      deallocate(mesh % edgeNormalVectors)

      if(associated(mesh % edgeTangentVectors % array)) then
         deallocate(mesh % edgeTangentVectors % array)
      end if
      deallocate(mesh % edgeTangentVectors % ioinfo)
      call mpas_deallocate_attlist(mesh % edgeTangentVectors % attList)
      deallocate(mesh % edgeTangentVectors)

      if(associated(mesh % localVerticalUnitVectors % array)) then
         deallocate(mesh % localVerticalUnitVectors % array)
      end if
      deallocate(mesh % localVerticalUnitVectors % ioinfo)
      call mpas_deallocate_attlist(mesh % localVerticalUnitVectors % attList)
      deallocate(mesh % localVerticalUnitVectors)

      if(associated(mesh % cellTangentPlane % array)) then
         deallocate(mesh % cellTangentPlane % array)
      end if
      deallocate(mesh % cellTangentPlane % ioinfo)
      call mpas_deallocate_attlist(mesh % cellTangentPlane % attList)
      deallocate(mesh % cellTangentPlane)

      if(associated(mesh % cellsOnCell % array)) then
         deallocate(mesh % cellsOnCell % array)
      end if
      deallocate(mesh % cellsOnCell % ioinfo)
      call mpas_deallocate_attlist(mesh % cellsOnCell % attList)
      deallocate(mesh % cellsOnCell)

      if(associated(mesh % verticesOnCell % array)) then
         deallocate(mesh % verticesOnCell % array)
      end if
      deallocate(mesh % verticesOnCell % ioinfo)
      call mpas_deallocate_attlist(mesh % verticesOnCell % attList)
      deallocate(mesh % verticesOnCell)

      if(associated(mesh % verticesOnEdge % array)) then
         deallocate(mesh % verticesOnEdge % array)
      end if
      deallocate(mesh % verticesOnEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % verticesOnEdge % attList)
      deallocate(mesh % verticesOnEdge)

      if(associated(mesh % edgesOnVertex % array)) then
         deallocate(mesh % edgesOnVertex % array)
      end if
      deallocate(mesh % edgesOnVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % edgesOnVertex % attList)
      deallocate(mesh % edgesOnVertex)

      if(associated(mesh % cellsOnVertex % array)) then
         deallocate(mesh % cellsOnVertex % array)
      end if
      deallocate(mesh % cellsOnVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % cellsOnVertex % attList)
      deallocate(mesh % cellsOnVertex)

      if(associated(mesh % kiteAreasOnVertex % array)) then
         deallocate(mesh % kiteAreasOnVertex % array)
      end if
      deallocate(mesh % kiteAreasOnVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % kiteAreasOnVertex % attList)
      deallocate(mesh % kiteAreasOnVertex)

      if(associated(mesh % fEdge % array)) then
         deallocate(mesh % fEdge % array)
      end if
      deallocate(mesh % fEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % fEdge % attList)
      deallocate(mesh % fEdge)

      if(associated(mesh % fVertex % array)) then
         deallocate(mesh % fVertex % array)
      end if
      deallocate(mesh % fVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % fVertex % attList)
      deallocate(mesh % fVertex)

      if(associated(mesh % fCell % array)) then
         deallocate(mesh % fCell % array)
      end if
      deallocate(mesh % fCell % ioinfo)
      call mpas_deallocate_attlist(mesh % fCell % attList)
      deallocate(mesh % fCell)

      if(associated(mesh % bottomDepth % array)) then
         deallocate(mesh % bottomDepth % array)
      end if
      deallocate(mesh % bottomDepth % ioinfo)
      call mpas_deallocate_attlist(mesh % bottomDepth % attList)
      deallocate(mesh % bottomDepth)

      if(associated(mesh % derivTwo % array)) then
         deallocate(mesh % derivTwo % array)
      end if
      deallocate(mesh % derivTwo % ioinfo)
      call mpas_deallocate_attlist(mesh % derivTwo % attList)
      deallocate(mesh % derivTwo)

      if(associated(mesh % advCoefs % array)) then
         deallocate(mesh % advCoefs % array)
      end if
      deallocate(mesh % advCoefs % ioinfo)
      call mpas_deallocate_attlist(mesh % advCoefs % attList)
      deallocate(mesh % advCoefs)

      if(associated(mesh % advCoefs3rd % array)) then
         deallocate(mesh % advCoefs3rd % array)
      end if
      deallocate(mesh % advCoefs3rd % ioinfo)
      call mpas_deallocate_attlist(mesh % advCoefs3rd % attList)
      deallocate(mesh % advCoefs3rd)

      if(associated(mesh % advCellsForEdge % array)) then
         deallocate(mesh % advCellsForEdge % array)
      end if
      deallocate(mesh % advCellsForEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % advCellsForEdge % attList)
      deallocate(mesh % advCellsForEdge)

      if(associated(mesh % nAdvCellsForEdge % array)) then
         deallocate(mesh % nAdvCellsForEdge % array)
      end if
      deallocate(mesh % nAdvCellsForEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % nAdvCellsForEdge % attList)
      deallocate(mesh % nAdvCellsForEdge)

      if(associated(mesh % highOrderAdvectionMask % array)) then
         deallocate(mesh % highOrderAdvectionMask % array)
      end if
      deallocate(mesh % highOrderAdvectionMask % ioinfo)
      call mpas_deallocate_attlist(mesh % highOrderAdvectionMask % attList)
      deallocate(mesh % highOrderAdvectionMask)

      if(associated(mesh % coeffs_reconstruct % array)) then
         deallocate(mesh % coeffs_reconstruct % array)
      end if
      deallocate(mesh % coeffs_reconstruct % ioinfo)
      call mpas_deallocate_attlist(mesh % coeffs_reconstruct % attList)
      deallocate(mesh % coeffs_reconstruct)

      if(associated(mesh % maxLevelCell % array)) then
         deallocate(mesh % maxLevelCell % array)
      end if
      deallocate(mesh % maxLevelCell % ioinfo)
      call mpas_deallocate_attlist(mesh % maxLevelCell % attList)
      deallocate(mesh % maxLevelCell)

      if(associated(mesh % maxLevelEdgeTop % array)) then
         deallocate(mesh % maxLevelEdgeTop % array)
      end if
      deallocate(mesh % maxLevelEdgeTop % ioinfo)
      call mpas_deallocate_attlist(mesh % maxLevelEdgeTop % attList)
      deallocate(mesh % maxLevelEdgeTop)

      if(associated(mesh % maxLevelEdgeBot % array)) then
         deallocate(mesh % maxLevelEdgeBot % array)
      end if
      deallocate(mesh % maxLevelEdgeBot % ioinfo)
      call mpas_deallocate_attlist(mesh % maxLevelEdgeBot % attList)
      deallocate(mesh % maxLevelEdgeBot)

      if(associated(mesh % maxLevelVertexTop % array)) then
         deallocate(mesh % maxLevelVertexTop % array)
      end if
      deallocate(mesh % maxLevelVertexTop % ioinfo)
      call mpas_deallocate_attlist(mesh % maxLevelVertexTop % attList)
      deallocate(mesh % maxLevelVertexTop)

      if(associated(mesh % maxLevelVertexBot % array)) then
         deallocate(mesh % maxLevelVertexBot % array)
      end if
      deallocate(mesh % maxLevelVertexBot % ioinfo)
      call mpas_deallocate_attlist(mesh % maxLevelVertexBot % attList)
      deallocate(mesh % maxLevelVertexBot)

      if(associated(mesh % refBottomDepth % array)) then
         deallocate(mesh % refBottomDepth % array)
      end if
      deallocate(mesh % refBottomDepth % ioinfo)
      call mpas_deallocate_attlist(mesh % refBottomDepth % attList)
      deallocate(mesh % refBottomDepth)

      if(associated(mesh % refBottomDepthTopOfCell % array)) then
         deallocate(mesh % refBottomDepthTopOfCell % array)
      end if
      deallocate(mesh % refBottomDepthTopOfCell % ioinfo)
      call mpas_deallocate_attlist(mesh % refBottomDepthTopOfCell % attList)
      deallocate(mesh % refBottomDepthTopOfCell)

      if(associated(mesh % vertCoordMovementWeights % array)) then
         deallocate(mesh % vertCoordMovementWeights % array)
      end if
      deallocate(mesh % vertCoordMovementWeights % ioinfo)
      call mpas_deallocate_attlist(mesh % vertCoordMovementWeights % attList)
      deallocate(mesh % vertCoordMovementWeights)

      if(associated(mesh % boundaryEdge % array)) then
         deallocate(mesh % boundaryEdge % array)
      end if
      deallocate(mesh % boundaryEdge % ioinfo)
      call mpas_deallocate_attlist(mesh % boundaryEdge % attList)
      deallocate(mesh % boundaryEdge)

      if(associated(mesh % boundaryVertex % array)) then
         deallocate(mesh % boundaryVertex % array)
      end if
      deallocate(mesh % boundaryVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % boundaryVertex % attList)
      deallocate(mesh % boundaryVertex)

      if(associated(mesh % boundaryCell % array)) then
         deallocate(mesh % boundaryCell % array)
      end if
      deallocate(mesh % boundaryCell % ioinfo)
      call mpas_deallocate_attlist(mesh % boundaryCell % attList)
      deallocate(mesh % boundaryCell)

      if(associated(mesh % edgeMask % array)) then
         deallocate(mesh % edgeMask % array)
      end if
      deallocate(mesh % edgeMask % ioinfo)
      call mpas_deallocate_attlist(mesh % edgeMask % attList)
      deallocate(mesh % edgeMask)

      if(associated(mesh % vertexMask % array)) then
         deallocate(mesh % vertexMask % array)
      end if
      deallocate(mesh % vertexMask % ioinfo)
      call mpas_deallocate_attlist(mesh % vertexMask % attList)
      deallocate(mesh % vertexMask)

      if(associated(mesh % cellMask % array)) then
         deallocate(mesh % cellMask % array)
      end if
      deallocate(mesh % cellMask % ioinfo)
      call mpas_deallocate_attlist(mesh % cellMask % attList)
      deallocate(mesh % cellMask)

      if(associated(mesh % temperatureRestore % array)) then
         deallocate(mesh % temperatureRestore % array)
      end if
      deallocate(mesh % temperatureRestore % ioinfo)
      call mpas_deallocate_attlist(mesh % temperatureRestore % attList)
      deallocate(mesh % temperatureRestore)

      if(associated(mesh % salinityRestore % array)) then
         deallocate(mesh % salinityRestore % array)
      end if
      deallocate(mesh % salinityRestore % ioinfo)
      call mpas_deallocate_attlist(mesh % salinityRestore % attList)
      deallocate(mesh % salinityRestore)

      if(associated(mesh % edgeSignOnCell % array)) then
         deallocate(mesh % edgeSignOnCell % array)
      end if
      deallocate(mesh % edgeSignOnCell % ioinfo)
      call mpas_deallocate_attlist(mesh % edgeSignOnCell % attList)
      deallocate(mesh % edgeSignOnCell)

      if(associated(mesh % edgeSignOnVertex % array)) then
         deallocate(mesh % edgeSignOnVertex % array)
      end if
      deallocate(mesh % edgeSignOnVertex % ioinfo)
      call mpas_deallocate_attlist(mesh % edgeSignOnVertex % attList)
      deallocate(mesh % edgeSignOnVertex)

      if(associated(mesh % kiteIndexOnCell % array)) then
         deallocate(mesh % kiteIndexOnCell % array)
      end if
      deallocate(mesh % kiteIndexOnCell % ioinfo)
      call mpas_deallocate_attlist(mesh % kiteIndexOnCell % attList)
      deallocate(mesh % kiteIndexOnCell)

   end subroutine mpas_deallocate_mesh


   subroutine mpas_deallocate_verticalMesh(verticalMesh)

      implicit none

      type (verticalMesh_type), intent(inout) :: verticalMesh

      if(associated(verticalMesh % restingThickness % array)) then
         deallocate(verticalMesh % restingThickness % array)
      end if
      deallocate(verticalMesh % restingThickness % ioinfo)
      call mpas_deallocate_attlist(verticalMesh % restingThickness % attList)
      deallocate(verticalMesh % restingThickness)

      if(associated(verticalMesh % refZMid % array)) then
         deallocate(verticalMesh % refZMid % array)
      end if
      deallocate(verticalMesh % refZMid % ioinfo)
      call mpas_deallocate_attlist(verticalMesh % refZMid % attList)
      deallocate(verticalMesh % refZMid)

      if(associated(verticalMesh % refLayerThickness % array)) then
         deallocate(verticalMesh % refLayerThickness % array)
      end if
      deallocate(verticalMesh % refLayerThickness % ioinfo)
      call mpas_deallocate_attlist(verticalMesh % refLayerThickness % attList)
      deallocate(verticalMesh % refLayerThickness)

   end subroutine mpas_deallocate_verticalMesh


   subroutine mpas_deallocate_tend(tend)

      implicit none

      type (tend_type), intent(inout) :: tend

      if(associated(tend % tracers % array)) then
         deallocate(tend % tracers % array)
      end if
      deallocate(tend % tracers % ioinfo)
      call mpas_deallocate_attlist(tend % tracers % attList)
      deallocate(tend % tracers)

      if(associated(tend % normalVelocity % array)) then
         deallocate(tend % normalVelocity % array)
      end if
      deallocate(tend % normalVelocity % ioinfo)
      call mpas_deallocate_attlist(tend % normalVelocity % attList)
      deallocate(tend % normalVelocity)

      if(associated(tend % layerThickness % array)) then
         deallocate(tend % layerThickness % array)
      end if
      deallocate(tend % layerThickness % ioinfo)
      call mpas_deallocate_attlist(tend % layerThickness % attList)
      deallocate(tend % layerThickness)

      if(associated(tend % ssh % array)) then
         deallocate(tend % ssh % array)
      end if
      deallocate(tend % ssh % ioinfo)
      call mpas_deallocate_attlist(tend % ssh % attList)
      deallocate(tend % ssh)

      if(associated(tend % highFreqThickness % array)) then
         deallocate(tend % highFreqThickness % array)
      end if
      deallocate(tend % highFreqThickness % ioinfo)
      call mpas_deallocate_attlist(tend % highFreqThickness % attList)
      deallocate(tend % highFreqThickness)

      if(associated(tend % lowFreqDivergence % array)) then
         deallocate(tend % lowFreqDivergence % array)
      end if
      deallocate(tend % lowFreqDivergence % ioinfo)
      call mpas_deallocate_attlist(tend % lowFreqDivergence % attList)
      deallocate(tend % lowFreqDivergence)

   end subroutine mpas_deallocate_tend


   subroutine mpas_deallocate_diagnostics(diagnostics)

      implicit none

      type (diagnostics_type), intent(inout) :: diagnostics

      if(associated(diagnostics % tracersSurfaceValue % array)) then
         deallocate(diagnostics % tracersSurfaceValue % array)
      end if
      deallocate(diagnostics % tracersSurfaceValue % ioinfo)
      call mpas_deallocate_attlist(diagnostics % tracersSurfaceValue % attList)
      deallocate(diagnostics % tracersSurfaceValue)

      if(associated(diagnostics % surfaceVelocity % array)) then
         deallocate(diagnostics % surfaceVelocity % array)
      end if
      deallocate(diagnostics % surfaceVelocity % ioinfo)
      call mpas_deallocate_attlist(diagnostics % surfaceVelocity % attList)
      deallocate(diagnostics % surfaceVelocity)

      if(associated(diagnostics % SSHGradient % array)) then
         deallocate(diagnostics % SSHGradient % array)
      end if
      deallocate(diagnostics % SSHGradient % ioinfo)
      call mpas_deallocate_attlist(diagnostics % SSHGradient % attList)
      deallocate(diagnostics % SSHGradient)

      deallocate(diagnostics % xtime % ioinfo)
      call mpas_deallocate_attlist(diagnostics % xtime % attList)
      deallocate(diagnostics % xtime)

      if(associated(diagnostics % zMid % array)) then
         deallocate(diagnostics % zMid % array)
      end if
      deallocate(diagnostics % zMid % ioinfo)
      call mpas_deallocate_attlist(diagnostics % zMid % attList)
      deallocate(diagnostics % zMid)

      if(associated(diagnostics % zTop % array)) then
         deallocate(diagnostics % zTop % array)
      end if
      deallocate(diagnostics % zTop % ioinfo)
      call mpas_deallocate_attlist(diagnostics % zTop % attList)
      deallocate(diagnostics % zTop)

      if(associated(diagnostics % density % array)) then
         deallocate(diagnostics % density % array)
      end if
      deallocate(diagnostics % density % ioinfo)
      call mpas_deallocate_attlist(diagnostics % density % attList)
      deallocate(diagnostics % density)

      if(associated(diagnostics % displacedDensity % array)) then
         deallocate(diagnostics % displacedDensity % array)
      end if
      deallocate(diagnostics % displacedDensity % ioinfo)
      call mpas_deallocate_attlist(diagnostics % displacedDensity % attList)
      deallocate(diagnostics % displacedDensity)

      if(associated(diagnostics % potentialDensity % array)) then
         deallocate(diagnostics % potentialDensity % array)
      end if
      deallocate(diagnostics % potentialDensity % ioinfo)
      call mpas_deallocate_attlist(diagnostics % potentialDensity % attList)
      deallocate(diagnostics % potentialDensity)

      if(associated(diagnostics % BruntVaisalaFreqTop % array)) then
         deallocate(diagnostics % BruntVaisalaFreqTop % array)
      end if
      deallocate(diagnostics % BruntVaisalaFreqTop % ioinfo)
      call mpas_deallocate_attlist(diagnostics % BruntVaisalaFreqTop % attList)
      deallocate(diagnostics % BruntVaisalaFreqTop)

      if(associated(diagnostics % montgomeryPotential % array)) then
         deallocate(diagnostics % montgomeryPotential % array)
      end if
      deallocate(diagnostics % montgomeryPotential % ioinfo)
      call mpas_deallocate_attlist(diagnostics % montgomeryPotential % attList)
      deallocate(diagnostics % montgomeryPotential)

      if(associated(diagnostics % pressure % array)) then
         deallocate(diagnostics % pressure % array)
      end if
      deallocate(diagnostics % pressure % ioinfo)
      call mpas_deallocate_attlist(diagnostics % pressure % attList)
      deallocate(diagnostics % pressure)

      if(associated(diagnostics % uTransport % array)) then
         deallocate(diagnostics % uTransport % array)
      end if
      deallocate(diagnostics % uTransport % ioinfo)
      call mpas_deallocate_attlist(diagnostics % uTransport % attList)
      deallocate(diagnostics % uTransport)

      if(associated(diagnostics % vertTransportVelocityTop % array)) then
         deallocate(diagnostics % vertTransportVelocityTop % array)
      end if
      deallocate(diagnostics % vertTransportVelocityTop % ioinfo)
      call mpas_deallocate_attlist(diagnostics % vertTransportVelocityTop % attList)
      deallocate(diagnostics % vertTransportVelocityTop)

      if(associated(diagnostics % vertVelocityTop % array)) then
         deallocate(diagnostics % vertVelocityTop % array)
      end if
      deallocate(diagnostics % vertVelocityTop % ioinfo)
      call mpas_deallocate_attlist(diagnostics % vertVelocityTop % attList)
      deallocate(diagnostics % vertVelocityTop)

      if(associated(diagnostics % tangentialVelocity % array)) then
         deallocate(diagnostics % tangentialVelocity % array)
      end if
      deallocate(diagnostics % tangentialVelocity % ioinfo)
      call mpas_deallocate_attlist(diagnostics % tangentialVelocity % attList)
      deallocate(diagnostics % tangentialVelocity)

      if(associated(diagnostics % layerThicknessEdge % array)) then
         deallocate(diagnostics % layerThicknessEdge % array)
      end if
      deallocate(diagnostics % layerThicknessEdge % ioinfo)
      call mpas_deallocate_attlist(diagnostics % layerThicknessEdge % attList)
      deallocate(diagnostics % layerThicknessEdge)

      if(associated(diagnostics % layerThicknessVertex % array)) then
         deallocate(diagnostics % layerThicknessVertex % array)
      end if
      deallocate(diagnostics % layerThicknessVertex % ioinfo)
      call mpas_deallocate_attlist(diagnostics % layerThicknessVertex % attList)
      deallocate(diagnostics % layerThicknessVertex)

      if(associated(diagnostics % kineticEnergyCell % array)) then
         deallocate(diagnostics % kineticEnergyCell % array)
      end if
      deallocate(diagnostics % kineticEnergyCell % ioinfo)
      call mpas_deallocate_attlist(diagnostics % kineticEnergyCell % attList)
      deallocate(diagnostics % kineticEnergyCell)

      if(associated(diagnostics % hEddyFlux % array)) then
         deallocate(diagnostics % hEddyFlux % array)
      end if
      deallocate(diagnostics % hEddyFlux % ioinfo)
      call mpas_deallocate_attlist(diagnostics % hEddyFlux % attList)
      deallocate(diagnostics % hEddyFlux)

      if(associated(diagnostics % hKappa % array)) then
         deallocate(diagnostics % hKappa % array)
      end if
      deallocate(diagnostics % hKappa % ioinfo)
      call mpas_deallocate_attlist(diagnostics % hKappa % attList)
      deallocate(diagnostics % hKappa)

      if(associated(diagnostics % hKappaQ % array)) then
         deallocate(diagnostics % hKappaQ % array)
      end if
      deallocate(diagnostics % hKappaQ % ioinfo)
      call mpas_deallocate_attlist(diagnostics % hKappaQ % attList)
      deallocate(diagnostics % hKappaQ)

      if(associated(diagnostics % viscosity % array)) then
         deallocate(diagnostics % viscosity % array)
      end if
      deallocate(diagnostics % viscosity % ioinfo)
      call mpas_deallocate_attlist(diagnostics % viscosity % attList)
      deallocate(diagnostics % viscosity)

      if(associated(diagnostics % divergence % array)) then
         deallocate(diagnostics % divergence % array)
      end if
      deallocate(diagnostics % divergence % ioinfo)
      call mpas_deallocate_attlist(diagnostics % divergence % attList)
      deallocate(diagnostics % divergence)

      if(associated(diagnostics % circulation % array)) then
         deallocate(diagnostics % circulation % array)
      end if
      deallocate(diagnostics % circulation % ioinfo)
      call mpas_deallocate_attlist(diagnostics % circulation % attList)
      deallocate(diagnostics % circulation)

      if(associated(diagnostics % relativeVorticity % array)) then
         deallocate(diagnostics % relativeVorticity % array)
      end if
      deallocate(diagnostics % relativeVorticity % ioinfo)
      call mpas_deallocate_attlist(diagnostics % relativeVorticity % attList)
      deallocate(diagnostics % relativeVorticity)

      if(associated(diagnostics % relativeVorticityCell % array)) then
         deallocate(diagnostics % relativeVorticityCell % array)
      end if
      deallocate(diagnostics % relativeVorticityCell % ioinfo)
      call mpas_deallocate_attlist(diagnostics % relativeVorticityCell % attList)
      deallocate(diagnostics % relativeVorticityCell)

      if(associated(diagnostics % normalizedRelativeVorticityEdge % array)) then
         deallocate(diagnostics % normalizedRelativeVorticityEdge % array)
      end if
      deallocate(diagnostics % normalizedRelativeVorticityEdge % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalizedRelativeVorticityEdge % attList)
      deallocate(diagnostics % normalizedRelativeVorticityEdge)

      if(associated(diagnostics % normalizedPlanetaryVorticityEdge % array)) then
         deallocate(diagnostics % normalizedPlanetaryVorticityEdge % array)
      end if
      deallocate(diagnostics % normalizedPlanetaryVorticityEdge % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalizedPlanetaryVorticityEdge % attList)
      deallocate(diagnostics % normalizedPlanetaryVorticityEdge)

      if(associated(diagnostics % normalizedRelativeVorticityCell % array)) then
         deallocate(diagnostics % normalizedRelativeVorticityCell % array)
      end if
      deallocate(diagnostics % normalizedRelativeVorticityCell % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalizedRelativeVorticityCell % attList)
      deallocate(diagnostics % normalizedRelativeVorticityCell)

      if(associated(diagnostics % barotropicForcing % array)) then
         deallocate(diagnostics % barotropicForcing % array)
      end if
      deallocate(diagnostics % barotropicForcing % ioinfo)
      call mpas_deallocate_attlist(diagnostics % barotropicForcing % attList)
      deallocate(diagnostics % barotropicForcing)

      if(associated(diagnostics % barotropicThicknessFlux % array)) then
         deallocate(diagnostics % barotropicThicknessFlux % array)
      end if
      deallocate(diagnostics % barotropicThicknessFlux % ioinfo)
      call mpas_deallocate_attlist(diagnostics % barotropicThicknessFlux % attList)
      deallocate(diagnostics % barotropicThicknessFlux)

      if(associated(diagnostics % normalVelocityX % array)) then
         deallocate(diagnostics % normalVelocityX % array)
      end if
      deallocate(diagnostics % normalVelocityX % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalVelocityX % attList)
      deallocate(diagnostics % normalVelocityX)

      if(associated(diagnostics % normalVelocityY % array)) then
         deallocate(diagnostics % normalVelocityY % array)
      end if
      deallocate(diagnostics % normalVelocityY % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalVelocityY % attList)
      deallocate(diagnostics % normalVelocityY)

      if(associated(diagnostics % normalVelocityZ % array)) then
         deallocate(diagnostics % normalVelocityZ % array)
      end if
      deallocate(diagnostics % normalVelocityZ % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalVelocityZ % attList)
      deallocate(diagnostics % normalVelocityZ)

      if(associated(diagnostics % normalVelocityZonal % array)) then
         deallocate(diagnostics % normalVelocityZonal % array)
      end if
      deallocate(diagnostics % normalVelocityZonal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalVelocityZonal % attList)
      deallocate(diagnostics % normalVelocityZonal)

      if(associated(diagnostics % normalVelocityMeridional % array)) then
         deallocate(diagnostics % normalVelocityMeridional % array)
      end if
      deallocate(diagnostics % normalVelocityMeridional % ioinfo)
      call mpas_deallocate_attlist(diagnostics % normalVelocityMeridional % attList)
      deallocate(diagnostics % normalVelocityMeridional)

      if(associated(diagnostics % gradSSH % array)) then
         deallocate(diagnostics % gradSSH % array)
      end if
      deallocate(diagnostics % gradSSH % ioinfo)
      call mpas_deallocate_attlist(diagnostics % gradSSH % attList)
      deallocate(diagnostics % gradSSH)

      if(associated(diagnostics % gradSSHX % array)) then
         deallocate(diagnostics % gradSSHX % array)
      end if
      deallocate(diagnostics % gradSSHX % ioinfo)
      call mpas_deallocate_attlist(diagnostics % gradSSHX % attList)
      deallocate(diagnostics % gradSSHX)

      if(associated(diagnostics % gradSSHY % array)) then
         deallocate(diagnostics % gradSSHY % array)
      end if
      deallocate(diagnostics % gradSSHY % ioinfo)
      call mpas_deallocate_attlist(diagnostics % gradSSHY % attList)
      deallocate(diagnostics % gradSSHY)

      if(associated(diagnostics % gradSSHZ % array)) then
         deallocate(diagnostics % gradSSHZ % array)
      end if
      deallocate(diagnostics % gradSSHZ % ioinfo)
      call mpas_deallocate_attlist(diagnostics % gradSSHZ % attList)
      deallocate(diagnostics % gradSSHZ)

      if(associated(diagnostics % gradSSHZonal % array)) then
         deallocate(diagnostics % gradSSHZonal % array)
      end if
      deallocate(diagnostics % gradSSHZonal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % gradSSHZonal % attList)
      deallocate(diagnostics % gradSSHZonal)

      if(associated(diagnostics % gradSSHMeridional % array)) then
         deallocate(diagnostics % gradSSHMeridional % array)
      end if
      deallocate(diagnostics % gradSSHMeridional % ioinfo)
      call mpas_deallocate_attlist(diagnostics % gradSSHMeridional % attList)
      deallocate(diagnostics % gradSSHMeridional)

      if(associated(diagnostics % uBolusGM % array)) then
         deallocate(diagnostics % uBolusGM % array)
      end if
      deallocate(diagnostics % uBolusGM % ioinfo)
      call mpas_deallocate_attlist(diagnostics % uBolusGM % attList)
      deallocate(diagnostics % uBolusGM)

      if(associated(diagnostics % uBolusGMX % array)) then
         deallocate(diagnostics % uBolusGMX % array)
      end if
      deallocate(diagnostics % uBolusGMX % ioinfo)
      call mpas_deallocate_attlist(diagnostics % uBolusGMX % attList)
      deallocate(diagnostics % uBolusGMX)

      if(associated(diagnostics % uBolusGMY % array)) then
         deallocate(diagnostics % uBolusGMY % array)
      end if
      deallocate(diagnostics % uBolusGMY % ioinfo)
      call mpas_deallocate_attlist(diagnostics % uBolusGMY % attList)
      deallocate(diagnostics % uBolusGMY)

      if(associated(diagnostics % uBolusGMZ % array)) then
         deallocate(diagnostics % uBolusGMZ % array)
      end if
      deallocate(diagnostics % uBolusGMZ % ioinfo)
      call mpas_deallocate_attlist(diagnostics % uBolusGMZ % attList)
      deallocate(diagnostics % uBolusGMZ)

      if(associated(diagnostics % uBolusGMZonal % array)) then
         deallocate(diagnostics % uBolusGMZonal % array)
      end if
      deallocate(diagnostics % uBolusGMZonal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % uBolusGMZonal % attList)
      deallocate(diagnostics % uBolusGMZonal)

      if(associated(diagnostics % uBolusGMMeridional % array)) then
         deallocate(diagnostics % uBolusGMMeridional % array)
      end if
      deallocate(diagnostics % uBolusGMMeridional % ioinfo)
      call mpas_deallocate_attlist(diagnostics % uBolusGMMeridional % attList)
      deallocate(diagnostics % uBolusGMMeridional)

      if(associated(diagnostics % RiTopOfCell % array)) then
         deallocate(diagnostics % RiTopOfCell % array)
      end if
      deallocate(diagnostics % RiTopOfCell % ioinfo)
      call mpas_deallocate_attlist(diagnostics % RiTopOfCell % attList)
      deallocate(diagnostics % RiTopOfCell)

      if(associated(diagnostics % RiTopOfEdge % array)) then
         deallocate(diagnostics % RiTopOfEdge % array)
      end if
      deallocate(diagnostics % RiTopOfEdge % ioinfo)
      call mpas_deallocate_attlist(diagnostics % RiTopOfEdge % attList)
      deallocate(diagnostics % RiTopOfEdge)

      if(associated(diagnostics % vertViscTopOfEdge % array)) then
         deallocate(diagnostics % vertViscTopOfEdge % array)
      end if
      deallocate(diagnostics % vertViscTopOfEdge % ioinfo)
      call mpas_deallocate_attlist(diagnostics % vertViscTopOfEdge % attList)
      deallocate(diagnostics % vertViscTopOfEdge)

      if(associated(diagnostics % vertViscTopOfCell % array)) then
         deallocate(diagnostics % vertViscTopOfCell % array)
      end if
      deallocate(diagnostics % vertViscTopOfCell % ioinfo)
      call mpas_deallocate_attlist(diagnostics % vertViscTopOfCell % attList)
      deallocate(diagnostics % vertViscTopOfCell)

      if(associated(diagnostics % vertDiffTopOfCell % array)) then
         deallocate(diagnostics % vertDiffTopOfCell % array)
      end if
      deallocate(diagnostics % vertDiffTopOfCell % ioinfo)
      call mpas_deallocate_attlist(diagnostics % vertDiffTopOfCell % attList)
      deallocate(diagnostics % vertDiffTopOfCell)

      if(associated(diagnostics % bulkRichardsonNumber % array)) then
         deallocate(diagnostics % bulkRichardsonNumber % array)
      end if
      deallocate(diagnostics % bulkRichardsonNumber % ioinfo)
      call mpas_deallocate_attlist(diagnostics % bulkRichardsonNumber % attList)
      deallocate(diagnostics % bulkRichardsonNumber)

      if(associated(diagnostics % boundaryLayerDepth % array)) then
         deallocate(diagnostics % boundaryLayerDepth % array)
      end if
      deallocate(diagnostics % boundaryLayerDepth % ioinfo)
      call mpas_deallocate_attlist(diagnostics % boundaryLayerDepth % attList)
      deallocate(diagnostics % boundaryLayerDepth)

      if(associated(diagnostics % indexBoundaryLayerDepth % array)) then
         deallocate(diagnostics % indexBoundaryLayerDepth % array)
      end if
      deallocate(diagnostics % indexBoundaryLayerDepth % ioinfo)
      call mpas_deallocate_attlist(diagnostics % indexBoundaryLayerDepth % attList)
      deallocate(diagnostics % indexBoundaryLayerDepth)

      if(associated(diagnostics % surfaceFrictionVelocity % array)) then
         deallocate(diagnostics % surfaceFrictionVelocity % array)
      end if
      deallocate(diagnostics % surfaceFrictionVelocity % ioinfo)
      call mpas_deallocate_attlist(diagnostics % surfaceFrictionVelocity % attList)
      deallocate(diagnostics % surfaceFrictionVelocity)

      if(associated(diagnostics % windStressZonalDiag % array)) then
         deallocate(diagnostics % windStressZonalDiag % array)
      end if
      deallocate(diagnostics % windStressZonalDiag % ioinfo)
      call mpas_deallocate_attlist(diagnostics % windStressZonalDiag % attList)
      deallocate(diagnostics % windStressZonalDiag)

      if(associated(diagnostics % windStressMeridionalDiag % array)) then
         deallocate(diagnostics % windStressMeridionalDiag % array)
      end if
      deallocate(diagnostics % windStressMeridionalDiag % ioinfo)
      call mpas_deallocate_attlist(diagnostics % windStressMeridionalDiag % attList)
      deallocate(diagnostics % windStressMeridionalDiag)

      if(associated(diagnostics % penetrativeTemperatureFluxOBL % array)) then
         deallocate(diagnostics % penetrativeTemperatureFluxOBL % array)
      end if
      deallocate(diagnostics % penetrativeTemperatureFluxOBL % ioinfo)
      call mpas_deallocate_attlist(diagnostics % penetrativeTemperatureFluxOBL % attList)
      deallocate(diagnostics % penetrativeTemperatureFluxOBL)

      if(associated(diagnostics % buoyancyForcingOBL % array)) then
         deallocate(diagnostics % buoyancyForcingOBL % array)
      end if
      deallocate(diagnostics % buoyancyForcingOBL % ioinfo)
      call mpas_deallocate_attlist(diagnostics % buoyancyForcingOBL % attList)
      deallocate(diagnostics % buoyancyForcingOBL)

      deallocate(diagnostics % areaCellGlobal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % areaCellGlobal % attList)
      deallocate(diagnostics % areaCellGlobal)

      deallocate(diagnostics % areaEdgeGlobal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % areaEdgeGlobal % attList)
      deallocate(diagnostics % areaEdgeGlobal)

      deallocate(diagnostics % areaTriangleGlobal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % areaTriangleGlobal % attList)
      deallocate(diagnostics % areaTriangleGlobal)

      deallocate(diagnostics % volumeCellGlobal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % volumeCellGlobal % attList)
      deallocate(diagnostics % volumeCellGlobal)

      deallocate(diagnostics % volumeEdgeGlobal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % volumeEdgeGlobal % attList)
      deallocate(diagnostics % volumeEdgeGlobal)

      deallocate(diagnostics % CFLNumberGlobal % ioinfo)
      call mpas_deallocate_attlist(diagnostics % CFLNumberGlobal % attList)
      deallocate(diagnostics % CFLNumberGlobal)

   end subroutine mpas_deallocate_diagnostics


   subroutine mpas_deallocate_average(average)

      implicit none

      type (average_type), intent(inout) :: average

      deallocate(average % nAverage % ioinfo)
      call mpas_deallocate_attlist(average % nAverage % attList)
      deallocate(average % nAverage)

      if(associated(average % avgSsh % array)) then
         deallocate(average % avgSsh % array)
      end if
      deallocate(average % avgSsh % ioinfo)
      call mpas_deallocate_attlist(average % avgSsh % attList)
      deallocate(average % avgSsh)

      if(associated(average % varSsh % array)) then
         deallocate(average % varSsh % array)
      end if
      deallocate(average % varSsh % ioinfo)
      call mpas_deallocate_attlist(average % varSsh % attList)
      deallocate(average % varSsh)

      if(associated(average % avgVelocityZonal % array)) then
         deallocate(average % avgVelocityZonal % array)
      end if
      deallocate(average % avgVelocityZonal % ioinfo)
      call mpas_deallocate_attlist(average % avgVelocityZonal % attList)
      deallocate(average % avgVelocityZonal)

      if(associated(average % avgVelocityMeridional % array)) then
         deallocate(average % avgVelocityMeridional % array)
      end if
      deallocate(average % avgVelocityMeridional % ioinfo)
      call mpas_deallocate_attlist(average % avgVelocityMeridional % attList)
      deallocate(average % avgVelocityMeridional)

      if(associated(average % varVelocityZonal % array)) then
         deallocate(average % varVelocityZonal % array)
      end if
      deallocate(average % varVelocityZonal % ioinfo)
      call mpas_deallocate_attlist(average % varVelocityZonal % attList)
      deallocate(average % varVelocityZonal)

      if(associated(average % varVelocityMeridional % array)) then
         deallocate(average % varVelocityMeridional % array)
      end if
      deallocate(average % varVelocityMeridional % ioinfo)
      call mpas_deallocate_attlist(average % varVelocityMeridional % attList)
      deallocate(average % varVelocityMeridional)

      if(associated(average % avgNormalVelocity % array)) then
         deallocate(average % avgNormalVelocity % array)
      end if
      deallocate(average % avgNormalVelocity % ioinfo)
      call mpas_deallocate_attlist(average % avgNormalVelocity % attList)
      deallocate(average % avgNormalVelocity)

      if(associated(average % varNormalVelocity % array)) then
         deallocate(average % varNormalVelocity % array)
      end if
      deallocate(average % varNormalVelocity % ioinfo)
      call mpas_deallocate_attlist(average % varNormalVelocity % attList)
      deallocate(average % varNormalVelocity)

      if(associated(average % avgVertVelocityTop % array)) then
         deallocate(average % avgVertVelocityTop % array)
      end if
      deallocate(average % avgVertVelocityTop % ioinfo)
      call mpas_deallocate_attlist(average % avgVertVelocityTop % attList)
      deallocate(average % avgVertVelocityTop)

   end subroutine mpas_deallocate_average


   subroutine mpas_deallocate_forcing(forcing)

      implicit none

      type (forcing_type), intent(inout) :: forcing

      if(associated(forcing % surfaceTracerFlux % array)) then
         deallocate(forcing % surfaceTracerFlux % array)
      end if
      deallocate(forcing % surfaceTracerFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % surfaceTracerFlux % attList)
      deallocate(forcing % surfaceTracerFlux)

      if(associated(forcing % avgTracersSurfaceValue % array)) then
         deallocate(forcing % avgTracersSurfaceValue % array)
      end if
      deallocate(forcing % avgTracersSurfaceValue % ioinfo)
      call mpas_deallocate_attlist(forcing % avgTracersSurfaceValue % attList)
      deallocate(forcing % avgTracersSurfaceValue)

      if(associated(forcing % avgSurfaceVelocity % array)) then
         deallocate(forcing % avgSurfaceVelocity % array)
      end if
      deallocate(forcing % avgSurfaceVelocity % ioinfo)
      call mpas_deallocate_attlist(forcing % avgSurfaceVelocity % attList)
      deallocate(forcing % avgSurfaceVelocity)

      if(associated(forcing % avgSSHGradient % array)) then
         deallocate(forcing % avgSSHGradient % array)
      end if
      deallocate(forcing % avgSSHGradient % ioinfo)
      call mpas_deallocate_attlist(forcing % avgSSHGradient % attList)
      deallocate(forcing % avgSSHGradient)

      if(associated(forcing % surfaceWindStress % array)) then
         deallocate(forcing % surfaceWindStress % array)
      end if
      deallocate(forcing % surfaceWindStress % ioinfo)
      call mpas_deallocate_attlist(forcing % surfaceWindStress % attList)
      deallocate(forcing % surfaceWindStress)

      if(associated(forcing % surfaceWindStressMagnitude % array)) then
         deallocate(forcing % surfaceWindStressMagnitude % array)
      end if
      deallocate(forcing % surfaceWindStressMagnitude % ioinfo)
      call mpas_deallocate_attlist(forcing % surfaceWindStressMagnitude % attList)
      deallocate(forcing % surfaceWindStressMagnitude)

      if(associated(forcing % surfaceMassFlux % array)) then
         deallocate(forcing % surfaceMassFlux % array)
      end if
      deallocate(forcing % surfaceMassFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % surfaceMassFlux % attList)
      deallocate(forcing % surfaceMassFlux)

      if(associated(forcing % seaSurfacePressure % array)) then
         deallocate(forcing % seaSurfacePressure % array)
      end if
      deallocate(forcing % seaSurfacePressure % ioinfo)
      call mpas_deallocate_attlist(forcing % seaSurfacePressure % attList)
      deallocate(forcing % seaSurfacePressure)

      if(associated(forcing % seaIceEnergy % array)) then
         deallocate(forcing % seaIceEnergy % array)
      end if
      deallocate(forcing % seaIceEnergy % ioinfo)
      call mpas_deallocate_attlist(forcing % seaIceEnergy % attList)
      deallocate(forcing % seaIceEnergy)

      if(associated(forcing % penetrativeTemperatureFlux % array)) then
         deallocate(forcing % penetrativeTemperatureFlux % array)
      end if
      deallocate(forcing % penetrativeTemperatureFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % penetrativeTemperatureFlux % attList)
      deallocate(forcing % penetrativeTemperatureFlux)

      if(associated(forcing % transmissionCoefficients % array)) then
         deallocate(forcing % transmissionCoefficients % array)
      end if
      deallocate(forcing % transmissionCoefficients % ioinfo)
      call mpas_deallocate_attlist(forcing % transmissionCoefficients % attList)
      deallocate(forcing % transmissionCoefficients)

      if(associated(forcing % windStressZonal % array)) then
         deallocate(forcing % windStressZonal % array)
      end if
      deallocate(forcing % windStressZonal % ioinfo)
      call mpas_deallocate_attlist(forcing % windStressZonal % attList)
      deallocate(forcing % windStressZonal)

      if(associated(forcing % windStressMeridional % array)) then
         deallocate(forcing % windStressMeridional % array)
      end if
      deallocate(forcing % windStressMeridional % ioinfo)
      call mpas_deallocate_attlist(forcing % windStressMeridional % attList)
      deallocate(forcing % windStressMeridional)

      if(associated(forcing % latentHeatFlux % array)) then
         deallocate(forcing % latentHeatFlux % array)
      end if
      deallocate(forcing % latentHeatFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % latentHeatFlux % attList)
      deallocate(forcing % latentHeatFlux)

      if(associated(forcing % sensibleHeatFlux % array)) then
         deallocate(forcing % sensibleHeatFlux % array)
      end if
      deallocate(forcing % sensibleHeatFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % sensibleHeatFlux % attList)
      deallocate(forcing % sensibleHeatFlux)

      if(associated(forcing % longWaveHeatFluxUp % array)) then
         deallocate(forcing % longWaveHeatFluxUp % array)
      end if
      deallocate(forcing % longWaveHeatFluxUp % ioinfo)
      call mpas_deallocate_attlist(forcing % longWaveHeatFluxUp % attList)
      deallocate(forcing % longWaveHeatFluxUp)

      if(associated(forcing % longWaveHeatFluxDown % array)) then
         deallocate(forcing % longWaveHeatFluxDown % array)
      end if
      deallocate(forcing % longWaveHeatFluxDown % ioinfo)
      call mpas_deallocate_attlist(forcing % longWaveHeatFluxDown % attList)
      deallocate(forcing % longWaveHeatFluxDown)

      if(associated(forcing % seaIceHeatFlux % array)) then
         deallocate(forcing % seaIceHeatFlux % array)
      end if
      deallocate(forcing % seaIceHeatFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % seaIceHeatFlux % attList)
      deallocate(forcing % seaIceHeatFlux)

      if(associated(forcing % shortWaveHeatFlux % array)) then
         deallocate(forcing % shortWaveHeatFlux % array)
      end if
      deallocate(forcing % shortWaveHeatFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % shortWaveHeatFlux % attList)
      deallocate(forcing % shortWaveHeatFlux)

      if(associated(forcing % evaporationFlux % array)) then
         deallocate(forcing % evaporationFlux % array)
      end if
      deallocate(forcing % evaporationFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % evaporationFlux % attList)
      deallocate(forcing % evaporationFlux)

      if(associated(forcing % seaIceSalinityFlux % array)) then
         deallocate(forcing % seaIceSalinityFlux % array)
      end if
      deallocate(forcing % seaIceSalinityFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % seaIceSalinityFlux % attList)
      deallocate(forcing % seaIceSalinityFlux)

      if(associated(forcing % seaIceFreshWaterFlux % array)) then
         deallocate(forcing % seaIceFreshWaterFlux % array)
      end if
      deallocate(forcing % seaIceFreshWaterFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % seaIceFreshWaterFlux % attList)
      deallocate(forcing % seaIceFreshWaterFlux)

      if(associated(forcing % riverRunoffFlux % array)) then
         deallocate(forcing % riverRunoffFlux % array)
      end if
      deallocate(forcing % riverRunoffFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % riverRunoffFlux % attList)
      deallocate(forcing % riverRunoffFlux)

      if(associated(forcing % iceRunoffFlux % array)) then
         deallocate(forcing % iceRunoffFlux % array)
      end if
      deallocate(forcing % iceRunoffFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % iceRunoffFlux % attList)
      deallocate(forcing % iceRunoffFlux)

      if(associated(forcing % rainFlux % array)) then
         deallocate(forcing % rainFlux % array)
      end if
      deallocate(forcing % rainFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % rainFlux % attList)
      deallocate(forcing % rainFlux)

      if(associated(forcing % snowFlux % array)) then
         deallocate(forcing % snowFlux % array)
      end if
      deallocate(forcing % snowFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % snowFlux % attList)
      deallocate(forcing % snowFlux)

      if(associated(forcing % iceFraction % array)) then
         deallocate(forcing % iceFraction % array)
      end if
      deallocate(forcing % iceFraction % ioinfo)
      call mpas_deallocate_attlist(forcing % iceFraction % attList)
      deallocate(forcing % iceFraction)

      if(associated(forcing % prognosticCO2 % array)) then
         deallocate(forcing % prognosticCO2 % array)
      end if
      deallocate(forcing % prognosticCO2 % ioinfo)
      call mpas_deallocate_attlist(forcing % prognosticCO2 % attList)
      deallocate(forcing % prognosticCO2)

      if(associated(forcing % diagnosticCO2 % array)) then
         deallocate(forcing % diagnosticCO2 % array)
      end if
      deallocate(forcing % diagnosticCO2 % ioinfo)
      call mpas_deallocate_attlist(forcing % diagnosticCO2 % attList)
      deallocate(forcing % diagnosticCO2)

      if(associated(forcing % squaredWindSpeed10Meter % array)) then
         deallocate(forcing % squaredWindSpeed10Meter % array)
      end if
      deallocate(forcing % squaredWindSpeed10Meter % ioinfo)
      call mpas_deallocate_attlist(forcing % squaredWindSpeed10Meter % attList)
      deallocate(forcing % squaredWindSpeed10Meter)

      deallocate(forcing % nAccumulatedCoupled % ioinfo)
      call mpas_deallocate_attlist(forcing % nAccumulatedCoupled % attList)
      deallocate(forcing % nAccumulatedCoupled)

      if(associated(forcing % CO2Flux % array)) then
         deallocate(forcing % CO2Flux % array)
      end if
      deallocate(forcing % CO2Flux % ioinfo)
      call mpas_deallocate_attlist(forcing % CO2Flux % attList)
      deallocate(forcing % CO2Flux)

      if(associated(forcing % DMSFlux % array)) then
         deallocate(forcing % DMSFlux % array)
      end if
      deallocate(forcing % DMSFlux % ioinfo)
      call mpas_deallocate_attlist(forcing % DMSFlux % attList)
      deallocate(forcing % DMSFlux)

   end subroutine mpas_deallocate_forcing


   subroutine mpas_deallocate_scratch(scratch)

      implicit none

      type (scratch_type), intent(inout) :: scratch

      if(associated(scratch % vorticityGradientTangentialComponent % array)) then
         deallocate(scratch % vorticityGradientTangentialComponent % array)
      end if
      deallocate(scratch % vorticityGradientTangentialComponent % ioinfo)
      call mpas_deallocate_attlist(scratch % vorticityGradientTangentialComponent % attList)
      deallocate(scratch % vorticityGradientTangentialComponent)

      if(associated(scratch % vorticityGradientNormalComponent % array)) then
         deallocate(scratch % vorticityGradientNormalComponent % array)
      end if
      deallocate(scratch % vorticityGradientNormalComponent % ioinfo)
      call mpas_deallocate_attlist(scratch % vorticityGradientNormalComponent % attList)
      deallocate(scratch % vorticityGradientNormalComponent)

      if(associated(scratch % normalizedRelativeVorticityVertex % array)) then
         deallocate(scratch % normalizedRelativeVorticityVertex % array)
      end if
      deallocate(scratch % normalizedRelativeVorticityVertex % ioinfo)
      call mpas_deallocate_attlist(scratch % normalizedRelativeVorticityVertex % attList)
      deallocate(scratch % normalizedRelativeVorticityVertex)

      if(associated(scratch % normalizedPlanetaryVorticityVertex % array)) then
         deallocate(scratch % normalizedPlanetaryVorticityVertex % array)
      end if
      deallocate(scratch % normalizedPlanetaryVorticityVertex % ioinfo)
      call mpas_deallocate_attlist(scratch % normalizedPlanetaryVorticityVertex % attList)
      deallocate(scratch % normalizedPlanetaryVorticityVertex)

      if(associated(scratch % kineticEnergyVertex % array)) then
         deallocate(scratch % kineticEnergyVertex % array)
      end if
      deallocate(scratch % kineticEnergyVertex % ioinfo)
      call mpas_deallocate_attlist(scratch % kineticEnergyVertex % attList)
      deallocate(scratch % kineticEnergyVertex)

      if(associated(scratch % kineticEnergyVertexOnCells % array)) then
         deallocate(scratch % kineticEnergyVertexOnCells % array)
      end if
      deallocate(scratch % kineticEnergyVertexOnCells % ioinfo)
      call mpas_deallocate_attlist(scratch % kineticEnergyVertexOnCells % attList)
      deallocate(scratch % kineticEnergyVertexOnCells)

      if(associated(scratch % densitySurfaceDisplaced % array)) then
         deallocate(scratch % densitySurfaceDisplaced % array)
      end if
      deallocate(scratch % densitySurfaceDisplaced % ioinfo)
      call mpas_deallocate_attlist(scratch % densitySurfaceDisplaced % attList)
      deallocate(scratch % densitySurfaceDisplaced)

      if(associated(scratch % thermalExpansionCoeff % array)) then
         deallocate(scratch % thermalExpansionCoeff % array)
      end if
      deallocate(scratch % thermalExpansionCoeff % ioinfo)
      call mpas_deallocate_attlist(scratch % thermalExpansionCoeff % attList)
      deallocate(scratch % thermalExpansionCoeff)

      if(associated(scratch % salineContractionCoeff % array)) then
         deallocate(scratch % salineContractionCoeff % array)
      end if
      deallocate(scratch % salineContractionCoeff % ioinfo)
      call mpas_deallocate_attlist(scratch % salineContractionCoeff % attList)
      deallocate(scratch % salineContractionCoeff)

      if(associated(scratch % normalVelocityTest % array)) then
         deallocate(scratch % normalVelocityTest % array)
      end if
      deallocate(scratch % normalVelocityTest % ioinfo)
      call mpas_deallocate_attlist(scratch % normalVelocityTest % attList)
      deallocate(scratch % normalVelocityTest)

      if(associated(scratch % tangentialVelocityTest % array)) then
         deallocate(scratch % tangentialVelocityTest % array)
      end if
      deallocate(scratch % tangentialVelocityTest % ioinfo)
      call mpas_deallocate_attlist(scratch % tangentialVelocityTest % attList)
      deallocate(scratch % tangentialVelocityTest)

      if(associated(scratch % strainRateR3Cell % array)) then
         deallocate(scratch % strainRateR3Cell % array)
      end if
      deallocate(scratch % strainRateR3Cell % ioinfo)
      call mpas_deallocate_attlist(scratch % strainRateR3Cell % attList)
      deallocate(scratch % strainRateR3Cell)

      if(associated(scratch % strainRateR3CellSolution % array)) then
         deallocate(scratch % strainRateR3CellSolution % array)
      end if
      deallocate(scratch % strainRateR3CellSolution % ioinfo)
      call mpas_deallocate_attlist(scratch % strainRateR3CellSolution % attList)
      deallocate(scratch % strainRateR3CellSolution)

      if(associated(scratch % strainRateR3Edge % array)) then
         deallocate(scratch % strainRateR3Edge % array)
      end if
      deallocate(scratch % strainRateR3Edge % ioinfo)
      call mpas_deallocate_attlist(scratch % strainRateR3Edge % attList)
      deallocate(scratch % strainRateR3Edge)

      if(associated(scratch % strainRateLonLatRCell % array)) then
         deallocate(scratch % strainRateLonLatRCell % array)
      end if
      deallocate(scratch % strainRateLonLatRCell % ioinfo)
      call mpas_deallocate_attlist(scratch % strainRateLonLatRCell % attList)
      deallocate(scratch % strainRateLonLatRCell)

      if(associated(scratch % strainRateLonLatRCellSolution % array)) then
         deallocate(scratch % strainRateLonLatRCellSolution % array)
      end if
      deallocate(scratch % strainRateLonLatRCellSolution % ioinfo)
      call mpas_deallocate_attlist(scratch % strainRateLonLatRCellSolution % attList)
      deallocate(scratch % strainRateLonLatRCellSolution)

      if(associated(scratch % strainRateLonLatREdge % array)) then
         deallocate(scratch % strainRateLonLatREdge % array)
      end if
      deallocate(scratch % strainRateLonLatREdge % ioinfo)
      call mpas_deallocate_attlist(scratch % strainRateLonLatREdge % attList)
      deallocate(scratch % strainRateLonLatREdge)

      if(associated(scratch % divTensorR3Cell % array)) then
         deallocate(scratch % divTensorR3Cell % array)
      end if
      deallocate(scratch % divTensorR3Cell % ioinfo)
      call mpas_deallocate_attlist(scratch % divTensorR3Cell % attList)
      deallocate(scratch % divTensorR3Cell)

      if(associated(scratch % divTensorR3CellSolution % array)) then
         deallocate(scratch % divTensorR3CellSolution % array)
      end if
      deallocate(scratch % divTensorR3CellSolution % ioinfo)
      call mpas_deallocate_attlist(scratch % divTensorR3CellSolution % attList)
      deallocate(scratch % divTensorR3CellSolution)

      if(associated(scratch % divTensorLonLatRCell % array)) then
         deallocate(scratch % divTensorLonLatRCell % array)
      end if
      deallocate(scratch % divTensorLonLatRCell % ioinfo)
      call mpas_deallocate_attlist(scratch % divTensorLonLatRCell % attList)
      deallocate(scratch % divTensorLonLatRCell)

      if(associated(scratch % divTensorLonLatRCellSolution % array)) then
         deallocate(scratch % divTensorLonLatRCellSolution % array)
      end if
      deallocate(scratch % divTensorLonLatRCellSolution % ioinfo)
      call mpas_deallocate_attlist(scratch % divTensorLonLatRCellSolution % attList)
      deallocate(scratch % divTensorLonLatRCellSolution)

      if(associated(scratch % outerProductEdge % array)) then
         deallocate(scratch % outerProductEdge % array)
      end if
      deallocate(scratch % outerProductEdge % ioinfo)
      call mpas_deallocate_attlist(scratch % outerProductEdge % attList)
      deallocate(scratch % outerProductEdge)

      if(associated(scratch % normalVectorEdge % array)) then
         deallocate(scratch % normalVectorEdge % array)
      end if
      deallocate(scratch % normalVectorEdge % ioinfo)
      call mpas_deallocate_attlist(scratch % normalVectorEdge % attList)
      deallocate(scratch % normalVectorEdge)

      if(associated(scratch % tangentialVectorEdge % array)) then
         deallocate(scratch % tangentialVectorEdge % array)
      end if
      deallocate(scratch % tangentialVectorEdge % ioinfo)
      call mpas_deallocate_attlist(scratch % tangentialVectorEdge % attList)
      deallocate(scratch % tangentialVectorEdge)

      if(associated(scratch % windStressFull % array)) then
         deallocate(scratch % windStressFull % array)
      end if
      deallocate(scratch % windStressFull % ioinfo)
      call mpas_deallocate_attlist(scratch % windStressFull % attList)
      deallocate(scratch % windStressFull)

      if(associated(scratch % windStressX % array)) then
         deallocate(scratch % windStressX % array)
      end if
      deallocate(scratch % windStressX % ioinfo)
      call mpas_deallocate_attlist(scratch % windStressX % attList)
      deallocate(scratch % windStressX)

      if(associated(scratch % windStressY % array)) then
         deallocate(scratch % windStressY % array)
      end if
      deallocate(scratch % windStressY % ioinfo)
      call mpas_deallocate_attlist(scratch % windStressY % attList)
      deallocate(scratch % windStressY)

      if(associated(scratch % windStressZ % array)) then
         deallocate(scratch % windStressZ % array)
      end if
      deallocate(scratch % windStressZ % ioinfo)
      call mpas_deallocate_attlist(scratch % windStressZ % attList)
      deallocate(scratch % windStressZ)

      if(associated(scratch % windStressZonal % array)) then
         deallocate(scratch % windStressZonal % array)
      end if
      deallocate(scratch % windStressZonal % ioinfo)
      call mpas_deallocate_attlist(scratch % windStressZonal % attList)
      deallocate(scratch % windStressZonal)

      if(associated(scratch % windStressMeridional % array)) then
         deallocate(scratch % windStressMeridional % array)
      end if
      deallocate(scratch % windStressMeridional % ioinfo)
      call mpas_deallocate_attlist(scratch % windStressMeridional % attList)
      deallocate(scratch % windStressMeridional)

   end subroutine mpas_deallocate_scratch




   subroutine mpas_copy_state(dest, src)

      implicit none

      type (state_type), intent(in) :: src
      type (state_type), intent(inout) :: dest

      if (associated(dest % tracers % array) .and. associated(src % tracers % array)) then
         dest % tracers % array = src % tracers % array
      end if
      if (associated(dest % normalVelocity % array) .and. associated(src % normalVelocity % array)) then
         dest % normalVelocity % array = src % normalVelocity % array
      end if
      if (associated(dest % layerThickness % array) .and. associated(src % layerThickness % array)) then
         dest % layerThickness % array = src % layerThickness % array
      end if
      if (associated(dest % ssh % array) .and. associated(src % ssh % array)) then
         dest % ssh % array = src % ssh % array
      end if
      if (associated(dest % highFreqThickness % array) .and. associated(src % highFreqThickness % array)) then
         dest % highFreqThickness % array = src % highFreqThickness % array
      end if
      if (associated(dest % lowFreqDivergence % array) .and. associated(src % lowFreqDivergence % array)) then
         dest % lowFreqDivergence % array = src % lowFreqDivergence % array
      end if
      if (associated(dest % normalBarotropicVelocity % array) .and. associated(src % normalBarotropicVelocity % array)) then
         dest % normalBarotropicVelocity % array = src % normalBarotropicVelocity % array
      end if
      if (associated(dest % normalBarotropicVelocitySubcycle % array) .and. associated(src % normalBarotropicVelocitySubcycle % &
array)) then
         dest % normalBarotropicVelocitySubcycle % array = src % normalBarotropicVelocitySubcycle % array
      end if
      if (associated(dest % sshSubcycle % array) .and. associated(src % sshSubcycle % array)) then
         dest % sshSubcycle % array = src % sshSubcycle % array
      end if
      if (associated(dest % normalBaroclinicVelocity % array) .and. associated(src % normalBaroclinicVelocity % array)) then
         dest % normalBaroclinicVelocity % array = src % normalBaroclinicVelocity % array
      end if

   end subroutine mpas_copy_state


   subroutine mpas_copy_mesh(dest, src)

      implicit none

      type (mesh_type), intent(in) :: src
      type (mesh_type), intent(inout) :: dest

      if (associated(dest % latCell % array) .and. associated(src % latCell % array)) then
         dest % latCell % array = src % latCell % array
      end if
      if (associated(dest % lonCell % array) .and. associated(src % lonCell % array)) then
         dest % lonCell % array = src % lonCell % array
      end if
      if (associated(dest % xCell % array) .and. associated(src % xCell % array)) then
         dest % xCell % array = src % xCell % array
      end if
      if (associated(dest % yCell % array) .and. associated(src % yCell % array)) then
         dest % yCell % array = src % yCell % array
      end if
      if (associated(dest % zCell % array) .and. associated(src % zCell % array)) then
         dest % zCell % array = src % zCell % array
      end if
      if (associated(dest % indexToCellID % array) .and. associated(src % indexToCellID % array)) then
         dest % indexToCellID % array = src % indexToCellID % array
      end if
      if (associated(dest % latEdge % array) .and. associated(src % latEdge % array)) then
         dest % latEdge % array = src % latEdge % array
      end if
      if (associated(dest % lonEdge % array) .and. associated(src % lonEdge % array)) then
         dest % lonEdge % array = src % lonEdge % array
      end if
      if (associated(dest % xEdge % array) .and. associated(src % xEdge % array)) then
         dest % xEdge % array = src % xEdge % array
      end if
      if (associated(dest % yEdge % array) .and. associated(src % yEdge % array)) then
         dest % yEdge % array = src % yEdge % array
      end if
      if (associated(dest % zEdge % array) .and. associated(src % zEdge % array)) then
         dest % zEdge % array = src % zEdge % array
      end if
      if (associated(dest % indexToEdgeID % array) .and. associated(src % indexToEdgeID % array)) then
         dest % indexToEdgeID % array = src % indexToEdgeID % array
      end if
      if (associated(dest % latVertex % array) .and. associated(src % latVertex % array)) then
         dest % latVertex % array = src % latVertex % array
      end if
      if (associated(dest % lonVertex % array) .and. associated(src % lonVertex % array)) then
         dest % lonVertex % array = src % lonVertex % array
      end if
      if (associated(dest % xVertex % array) .and. associated(src % xVertex % array)) then
         dest % xVertex % array = src % xVertex % array
      end if
      if (associated(dest % yVertex % array) .and. associated(src % yVertex % array)) then
         dest % yVertex % array = src % yVertex % array
      end if
      if (associated(dest % zVertex % array) .and. associated(src % zVertex % array)) then
         dest % zVertex % array = src % zVertex % array
      end if
      if (associated(dest % indexToVertexID % array) .and. associated(src % indexToVertexID % array)) then
         dest % indexToVertexID % array = src % indexToVertexID % array
      end if
      if (associated(dest % meshDensity % array) .and. associated(src % meshDensity % array)) then
         dest % meshDensity % array = src % meshDensity % array
      end if
      if (associated(dest % meshScalingDel2 % array) .and. associated(src % meshScalingDel2 % array)) then
         dest % meshScalingDel2 % array = src % meshScalingDel2 % array
      end if
      if (associated(dest % meshScalingDel4 % array) .and. associated(src % meshScalingDel4 % array)) then
         dest % meshScalingDel4 % array = src % meshScalingDel4 % array
      end if
      if (associated(dest % meshScaling % array) .and. associated(src % meshScaling % array)) then
         dest % meshScaling % array = src % meshScaling % array
      end if
      if (associated(dest % cellsOnEdge % array) .and. associated(src % cellsOnEdge % array)) then
         dest % cellsOnEdge % array = src % cellsOnEdge % array
      end if
      if (associated(dest % nEdgesOnCell % array) .and. associated(src % nEdgesOnCell % array)) then
         dest % nEdgesOnCell % array = src % nEdgesOnCell % array
      end if
      if (associated(dest % nEdgesOnEdge % array) .and. associated(src % nEdgesOnEdge % array)) then
         dest % nEdgesOnEdge % array = src % nEdgesOnEdge % array
      end if
      if (associated(dest % edgesOnCell % array) .and. associated(src % edgesOnCell % array)) then
         dest % edgesOnCell % array = src % edgesOnCell % array
      end if
      if (associated(dest % edgesOnEdge % array) .and. associated(src % edgesOnEdge % array)) then
         dest % edgesOnEdge % array = src % edgesOnEdge % array
      end if
      if (associated(dest % weightsOnEdge % array) .and. associated(src % weightsOnEdge % array)) then
         dest % weightsOnEdge % array = src % weightsOnEdge % array
      end if
      if (associated(dest % dvEdge % array) .and. associated(src % dvEdge % array)) then
         dest % dvEdge % array = src % dvEdge % array
      end if
      if (associated(dest % dcEdge % array) .and. associated(src % dcEdge % array)) then
         dest % dcEdge % array = src % dcEdge % array
      end if
      if (associated(dest % angleEdge % array) .and. associated(src % angleEdge % array)) then
         dest % angleEdge % array = src % angleEdge % array
      end if
      if (associated(dest % areaCell % array) .and. associated(src % areaCell % array)) then
         dest % areaCell % array = src % areaCell % array
      end if
      if (associated(dest % areaTriangle % array) .and. associated(src % areaTriangle % array)) then
         dest % areaTriangle % array = src % areaTriangle % array
      end if
      if (associated(dest % edgeNormalVectors % array) .and. associated(src % edgeNormalVectors % array)) then
         dest % edgeNormalVectors % array = src % edgeNormalVectors % array
      end if
      if (associated(dest % edgeTangentVectors % array) .and. associated(src % edgeTangentVectors % array)) then
         dest % edgeTangentVectors % array = src % edgeTangentVectors % array
      end if
      if (associated(dest % localVerticalUnitVectors % array) .and. associated(src % localVerticalUnitVectors % array)) then
         dest % localVerticalUnitVectors % array = src % localVerticalUnitVectors % array
      end if
      if (associated(dest % cellTangentPlane % array) .and. associated(src % cellTangentPlane % array)) then
         dest % cellTangentPlane % array = src % cellTangentPlane % array
      end if
      if (associated(dest % cellsOnCell % array) .and. associated(src % cellsOnCell % array)) then
         dest % cellsOnCell % array = src % cellsOnCell % array
      end if
      if (associated(dest % verticesOnCell % array) .and. associated(src % verticesOnCell % array)) then
         dest % verticesOnCell % array = src % verticesOnCell % array
      end if
      if (associated(dest % verticesOnEdge % array) .and. associated(src % verticesOnEdge % array)) then
         dest % verticesOnEdge % array = src % verticesOnEdge % array
      end if
      if (associated(dest % edgesOnVertex % array) .and. associated(src % edgesOnVertex % array)) then
         dest % edgesOnVertex % array = src % edgesOnVertex % array
      end if
      if (associated(dest % cellsOnVertex % array) .and. associated(src % cellsOnVertex % array)) then
         dest % cellsOnVertex % array = src % cellsOnVertex % array
      end if
      if (associated(dest % kiteAreasOnVertex % array) .and. associated(src % kiteAreasOnVertex % array)) then
         dest % kiteAreasOnVertex % array = src % kiteAreasOnVertex % array
      end if
      if (associated(dest % fEdge % array) .and. associated(src % fEdge % array)) then
         dest % fEdge % array = src % fEdge % array
      end if
      if (associated(dest % fVertex % array) .and. associated(src % fVertex % array)) then
         dest % fVertex % array = src % fVertex % array
      end if
      if (associated(dest % fCell % array) .and. associated(src % fCell % array)) then
         dest % fCell % array = src % fCell % array
      end if
      if (associated(dest % bottomDepth % array) .and. associated(src % bottomDepth % array)) then
         dest % bottomDepth % array = src % bottomDepth % array
      end if
      if (associated(dest % derivTwo % array) .and. associated(src % derivTwo % array)) then
         dest % derivTwo % array = src % derivTwo % array
      end if
      if (associated(dest % advCoefs % array) .and. associated(src % advCoefs % array)) then
         dest % advCoefs % array = src % advCoefs % array
      end if
      if (associated(dest % advCoefs3rd % array) .and. associated(src % advCoefs3rd % array)) then
         dest % advCoefs3rd % array = src % advCoefs3rd % array
      end if
      if (associated(dest % advCellsForEdge % array) .and. associated(src % advCellsForEdge % array)) then
         dest % advCellsForEdge % array = src % advCellsForEdge % array
      end if
      if (associated(dest % nAdvCellsForEdge % array) .and. associated(src % nAdvCellsForEdge % array)) then
         dest % nAdvCellsForEdge % array = src % nAdvCellsForEdge % array
      end if
      if (associated(dest % highOrderAdvectionMask % array) .and. associated(src % highOrderAdvectionMask % array)) then
         dest % highOrderAdvectionMask % array = src % highOrderAdvectionMask % array
      end if
      if (associated(dest % coeffs_reconstruct % array) .and. associated(src % coeffs_reconstruct % array)) then
         dest % coeffs_reconstruct % array = src % coeffs_reconstruct % array
      end if
      if (associated(dest % maxLevelCell % array) .and. associated(src % maxLevelCell % array)) then
         dest % maxLevelCell % array = src % maxLevelCell % array
      end if
      if (associated(dest % maxLevelEdgeTop % array) .and. associated(src % maxLevelEdgeTop % array)) then
         dest % maxLevelEdgeTop % array = src % maxLevelEdgeTop % array
      end if
      if (associated(dest % maxLevelEdgeBot % array) .and. associated(src % maxLevelEdgeBot % array)) then
         dest % maxLevelEdgeBot % array = src % maxLevelEdgeBot % array
      end if
      if (associated(dest % maxLevelVertexTop % array) .and. associated(src % maxLevelVertexTop % array)) then
         dest % maxLevelVertexTop % array = src % maxLevelVertexTop % array
      end if
      if (associated(dest % maxLevelVertexBot % array) .and. associated(src % maxLevelVertexBot % array)) then
         dest % maxLevelVertexBot % array = src % maxLevelVertexBot % array
      end if
      if (associated(dest % refBottomDepth % array) .and. associated(src % refBottomDepth % array)) then
         dest % refBottomDepth % array = src % refBottomDepth % array
      end if
      if (associated(dest % refBottomDepthTopOfCell % array) .and. associated(src % refBottomDepthTopOfCell % array)) then
         dest % refBottomDepthTopOfCell % array = src % refBottomDepthTopOfCell % array
      end if
      if (associated(dest % vertCoordMovementWeights % array) .and. associated(src % vertCoordMovementWeights % array)) then
         dest % vertCoordMovementWeights % array = src % vertCoordMovementWeights % array
      end if
      if (associated(dest % boundaryEdge % array) .and. associated(src % boundaryEdge % array)) then
         dest % boundaryEdge % array = src % boundaryEdge % array
      end if
      if (associated(dest % boundaryVertex % array) .and. associated(src % boundaryVertex % array)) then
         dest % boundaryVertex % array = src % boundaryVertex % array
      end if
      if (associated(dest % boundaryCell % array) .and. associated(src % boundaryCell % array)) then
         dest % boundaryCell % array = src % boundaryCell % array
      end if
      if (associated(dest % edgeMask % array) .and. associated(src % edgeMask % array)) then
         dest % edgeMask % array = src % edgeMask % array
      end if
      if (associated(dest % vertexMask % array) .and. associated(src % vertexMask % array)) then
         dest % vertexMask % array = src % vertexMask % array
      end if
      if (associated(dest % cellMask % array) .and. associated(src % cellMask % array)) then
         dest % cellMask % array = src % cellMask % array
      end if
      if (associated(dest % temperatureRestore % array) .and. associated(src % temperatureRestore % array)) then
         dest % temperatureRestore % array = src % temperatureRestore % array
      end if
      if (associated(dest % salinityRestore % array) .and. associated(src % salinityRestore % array)) then
         dest % salinityRestore % array = src % salinityRestore % array
      end if
      if (associated(dest % edgeSignOnCell % array) .and. associated(src % edgeSignOnCell % array)) then
         dest % edgeSignOnCell % array = src % edgeSignOnCell % array
      end if
      if (associated(dest % edgeSignOnVertex % array) .and. associated(src % edgeSignOnVertex % array)) then
         dest % edgeSignOnVertex % array = src % edgeSignOnVertex % array
      end if
      if (associated(dest % kiteIndexOnCell % array) .and. associated(src % kiteIndexOnCell % array)) then
         dest % kiteIndexOnCell % array = src % kiteIndexOnCell % array
      end if

   end subroutine mpas_copy_mesh


   subroutine mpas_copy_verticalMesh(dest, src)

      implicit none

      type (verticalMesh_type), intent(in) :: src
      type (verticalMesh_type), intent(inout) :: dest

      if (associated(dest % restingThickness % array) .and. associated(src % restingThickness % array)) then
         dest % restingThickness % array = src % restingThickness % array
      end if
      if (associated(dest % refZMid % array) .and. associated(src % refZMid % array)) then
         dest % refZMid % array = src % refZMid % array
      end if
      if (associated(dest % refLayerThickness % array) .and. associated(src % refLayerThickness % array)) then
         dest % refLayerThickness % array = src % refLayerThickness % array
      end if

   end subroutine mpas_copy_verticalMesh


   subroutine mpas_copy_tend(dest, src)

      implicit none

      type (tend_type), intent(in) :: src
      type (tend_type), intent(inout) :: dest

      if (associated(dest % tracers % array) .and. associated(src % tracers % array)) then
         dest % tracers % array = src % tracers % array
      end if
      if (associated(dest % normalVelocity % array) .and. associated(src % normalVelocity % array)) then
         dest % normalVelocity % array = src % normalVelocity % array
      end if
      if (associated(dest % layerThickness % array) .and. associated(src % layerThickness % array)) then
         dest % layerThickness % array = src % layerThickness % array
      end if
      if (associated(dest % ssh % array) .and. associated(src % ssh % array)) then
         dest % ssh % array = src % ssh % array
      end if
      if (associated(dest % highFreqThickness % array) .and. associated(src % highFreqThickness % array)) then
         dest % highFreqThickness % array = src % highFreqThickness % array
      end if
      if (associated(dest % lowFreqDivergence % array) .and. associated(src % lowFreqDivergence % array)) then
         dest % lowFreqDivergence % array = src % lowFreqDivergence % array
      end if

   end subroutine mpas_copy_tend


   subroutine mpas_copy_diagnostics(dest, src)

      implicit none

      type (diagnostics_type), intent(in) :: src
      type (diagnostics_type), intent(inout) :: dest

      if (associated(dest % tracersSurfaceValue % array) .and. associated(src % tracersSurfaceValue % array)) then
         dest % tracersSurfaceValue % array = src % tracersSurfaceValue % array
      end if
      if (associated(dest % surfaceVelocity % array) .and. associated(src % surfaceVelocity % array)) then
         dest % surfaceVelocity % array = src % surfaceVelocity % array
      end if
      if (associated(dest % SSHGradient % array) .and. associated(src % SSHGradient % array)) then
         dest % SSHGradient % array = src % SSHGradient % array
      end if
      dest % xtime % scalar = src % xtime % scalar
      if (associated(dest % zMid % array) .and. associated(src % zMid % array)) then
         dest % zMid % array = src % zMid % array
      end if
      if (associated(dest % zTop % array) .and. associated(src % zTop % array)) then
         dest % zTop % array = src % zTop % array
      end if
      if (associated(dest % density % array) .and. associated(src % density % array)) then
         dest % density % array = src % density % array
      end if
      if (associated(dest % displacedDensity % array) .and. associated(src % displacedDensity % array)) then
         dest % displacedDensity % array = src % displacedDensity % array
      end if
      if (associated(dest % potentialDensity % array) .and. associated(src % potentialDensity % array)) then
         dest % potentialDensity % array = src % potentialDensity % array
      end if
      if (associated(dest % BruntVaisalaFreqTop % array) .and. associated(src % BruntVaisalaFreqTop % array)) then
         dest % BruntVaisalaFreqTop % array = src % BruntVaisalaFreqTop % array
      end if
      if (associated(dest % montgomeryPotential % array) .and. associated(src % montgomeryPotential % array)) then
         dest % montgomeryPotential % array = src % montgomeryPotential % array
      end if
      if (associated(dest % pressure % array) .and. associated(src % pressure % array)) then
         dest % pressure % array = src % pressure % array
      end if
      if (associated(dest % uTransport % array) .and. associated(src % uTransport % array)) then
         dest % uTransport % array = src % uTransport % array
      end if
      if (associated(dest % vertTransportVelocityTop % array) .and. associated(src % vertTransportVelocityTop % array)) then
         dest % vertTransportVelocityTop % array = src % vertTransportVelocityTop % array
      end if
      if (associated(dest % vertVelocityTop % array) .and. associated(src % vertVelocityTop % array)) then
         dest % vertVelocityTop % array = src % vertVelocityTop % array
      end if
      if (associated(dest % tangentialVelocity % array) .and. associated(src % tangentialVelocity % array)) then
         dest % tangentialVelocity % array = src % tangentialVelocity % array
      end if
      if (associated(dest % layerThicknessEdge % array) .and. associated(src % layerThicknessEdge % array)) then
         dest % layerThicknessEdge % array = src % layerThicknessEdge % array
      end if
      if (associated(dest % layerThicknessVertex % array) .and. associated(src % layerThicknessVertex % array)) then
         dest % layerThicknessVertex % array = src % layerThicknessVertex % array
      end if
      if (associated(dest % kineticEnergyCell % array) .and. associated(src % kineticEnergyCell % array)) then
         dest % kineticEnergyCell % array = src % kineticEnergyCell % array
      end if
      if (associated(dest % hEddyFlux % array) .and. associated(src % hEddyFlux % array)) then
         dest % hEddyFlux % array = src % hEddyFlux % array
      end if
      if (associated(dest % hKappa % array) .and. associated(src % hKappa % array)) then
         dest % hKappa % array = src % hKappa % array
      end if
      if (associated(dest % hKappaQ % array) .and. associated(src % hKappaQ % array)) then
         dest % hKappaQ % array = src % hKappaQ % array
      end if
      if (associated(dest % viscosity % array) .and. associated(src % viscosity % array)) then
         dest % viscosity % array = src % viscosity % array
      end if
      if (associated(dest % divergence % array) .and. associated(src % divergence % array)) then
         dest % divergence % array = src % divergence % array
      end if
      if (associated(dest % circulation % array) .and. associated(src % circulation % array)) then
         dest % circulation % array = src % circulation % array
      end if
      if (associated(dest % relativeVorticity % array) .and. associated(src % relativeVorticity % array)) then
         dest % relativeVorticity % array = src % relativeVorticity % array
      end if
      if (associated(dest % relativeVorticityCell % array) .and. associated(src % relativeVorticityCell % array)) then
         dest % relativeVorticityCell % array = src % relativeVorticityCell % array
      end if
      if (associated(dest % normalizedRelativeVorticityEdge % array) .and. associated(src % normalizedRelativeVorticityEdge % &
array)) then
         dest % normalizedRelativeVorticityEdge % array = src % normalizedRelativeVorticityEdge % array
      end if
      if (associated(dest % normalizedPlanetaryVorticityEdge % array) .and. associated(src % normalizedPlanetaryVorticityEdge % &
array)) then
         dest % normalizedPlanetaryVorticityEdge % array = src % normalizedPlanetaryVorticityEdge % array
      end if
      if (associated(dest % normalizedRelativeVorticityCell % array) .and. associated(src % normalizedRelativeVorticityCell % &
array)) then
         dest % normalizedRelativeVorticityCell % array = src % normalizedRelativeVorticityCell % array
      end if
      if (associated(dest % barotropicForcing % array) .and. associated(src % barotropicForcing % array)) then
         dest % barotropicForcing % array = src % barotropicForcing % array
      end if
      if (associated(dest % barotropicThicknessFlux % array) .and. associated(src % barotropicThicknessFlux % array)) then
         dest % barotropicThicknessFlux % array = src % barotropicThicknessFlux % array
      end if
      if (associated(dest % normalVelocityX % array) .and. associated(src % normalVelocityX % array)) then
         dest % normalVelocityX % array = src % normalVelocityX % array
      end if
      if (associated(dest % normalVelocityY % array) .and. associated(src % normalVelocityY % array)) then
         dest % normalVelocityY % array = src % normalVelocityY % array
      end if
      if (associated(dest % normalVelocityZ % array) .and. associated(src % normalVelocityZ % array)) then
         dest % normalVelocityZ % array = src % normalVelocityZ % array
      end if
      if (associated(dest % normalVelocityZonal % array) .and. associated(src % normalVelocityZonal % array)) then
         dest % normalVelocityZonal % array = src % normalVelocityZonal % array
      end if
      if (associated(dest % normalVelocityMeridional % array) .and. associated(src % normalVelocityMeridional % array)) then
         dest % normalVelocityMeridional % array = src % normalVelocityMeridional % array
      end if
      if (associated(dest % gradSSH % array) .and. associated(src % gradSSH % array)) then
         dest % gradSSH % array = src % gradSSH % array
      end if
      if (associated(dest % gradSSHX % array) .and. associated(src % gradSSHX % array)) then
         dest % gradSSHX % array = src % gradSSHX % array
      end if
      if (associated(dest % gradSSHY % array) .and. associated(src % gradSSHY % array)) then
         dest % gradSSHY % array = src % gradSSHY % array
      end if
      if (associated(dest % gradSSHZ % array) .and. associated(src % gradSSHZ % array)) then
         dest % gradSSHZ % array = src % gradSSHZ % array
      end if
      if (associated(dest % gradSSHZonal % array) .and. associated(src % gradSSHZonal % array)) then
         dest % gradSSHZonal % array = src % gradSSHZonal % array
      end if
      if (associated(dest % gradSSHMeridional % array) .and. associated(src % gradSSHMeridional % array)) then
         dest % gradSSHMeridional % array = src % gradSSHMeridional % array
      end if
      if (associated(dest % uBolusGM % array) .and. associated(src % uBolusGM % array)) then
         dest % uBolusGM % array = src % uBolusGM % array
      end if
      if (associated(dest % uBolusGMX % array) .and. associated(src % uBolusGMX % array)) then
         dest % uBolusGMX % array = src % uBolusGMX % array
      end if
      if (associated(dest % uBolusGMY % array) .and. associated(src % uBolusGMY % array)) then
         dest % uBolusGMY % array = src % uBolusGMY % array
      end if
      if (associated(dest % uBolusGMZ % array) .and. associated(src % uBolusGMZ % array)) then
         dest % uBolusGMZ % array = src % uBolusGMZ % array
      end if
      if (associated(dest % uBolusGMZonal % array) .and. associated(src % uBolusGMZonal % array)) then
         dest % uBolusGMZonal % array = src % uBolusGMZonal % array
      end if
      if (associated(dest % uBolusGMMeridional % array) .and. associated(src % uBolusGMMeridional % array)) then
         dest % uBolusGMMeridional % array = src % uBolusGMMeridional % array
      end if
      if (associated(dest % RiTopOfCell % array) .and. associated(src % RiTopOfCell % array)) then
         dest % RiTopOfCell % array = src % RiTopOfCell % array
      end if
      if (associated(dest % RiTopOfEdge % array) .and. associated(src % RiTopOfEdge % array)) then
         dest % RiTopOfEdge % array = src % RiTopOfEdge % array
      end if
      if (associated(dest % vertViscTopOfEdge % array) .and. associated(src % vertViscTopOfEdge % array)) then
         dest % vertViscTopOfEdge % array = src % vertViscTopOfEdge % array
      end if
      if (associated(dest % vertViscTopOfCell % array) .and. associated(src % vertViscTopOfCell % array)) then
         dest % vertViscTopOfCell % array = src % vertViscTopOfCell % array
      end if
      if (associated(dest % vertDiffTopOfCell % array) .and. associated(src % vertDiffTopOfCell % array)) then
         dest % vertDiffTopOfCell % array = src % vertDiffTopOfCell % array
      end if
      if (associated(dest % bulkRichardsonNumber % array) .and. associated(src % bulkRichardsonNumber % array)) then
         dest % bulkRichardsonNumber % array = src % bulkRichardsonNumber % array
      end if
      if (associated(dest % boundaryLayerDepth % array) .and. associated(src % boundaryLayerDepth % array)) then
         dest % boundaryLayerDepth % array = src % boundaryLayerDepth % array
      end if
      if (associated(dest % indexBoundaryLayerDepth % array) .and. associated(src % indexBoundaryLayerDepth % array)) then
         dest % indexBoundaryLayerDepth % array = src % indexBoundaryLayerDepth % array
      end if
      if (associated(dest % surfaceFrictionVelocity % array) .and. associated(src % surfaceFrictionVelocity % array)) then
         dest % surfaceFrictionVelocity % array = src % surfaceFrictionVelocity % array
      end if
      if (associated(dest % windStressZonalDiag % array) .and. associated(src % windStressZonalDiag % array)) then
         dest % windStressZonalDiag % array = src % windStressZonalDiag % array
      end if
      if (associated(dest % windStressMeridionalDiag % array) .and. associated(src % windStressMeridionalDiag % array)) then
         dest % windStressMeridionalDiag % array = src % windStressMeridionalDiag % array
      end if
      if (associated(dest % penetrativeTemperatureFluxOBL % array) .and. associated(src % penetrativeTemperatureFluxOBL % array)) &
then
         dest % penetrativeTemperatureFluxOBL % array = src % penetrativeTemperatureFluxOBL % array
      end if
      if (associated(dest % buoyancyForcingOBL % array) .and. associated(src % buoyancyForcingOBL % array)) then
         dest % buoyancyForcingOBL % array = src % buoyancyForcingOBL % array
      end if
      dest % areaCellGlobal % scalar = src % areaCellGlobal % scalar
      dest % areaEdgeGlobal % scalar = src % areaEdgeGlobal % scalar
      dest % areaTriangleGlobal % scalar = src % areaTriangleGlobal % scalar
      dest % volumeCellGlobal % scalar = src % volumeCellGlobal % scalar
      dest % volumeEdgeGlobal % scalar = src % volumeEdgeGlobal % scalar
      dest % CFLNumberGlobal % scalar = src % CFLNumberGlobal % scalar

   end subroutine mpas_copy_diagnostics


   subroutine mpas_copy_average(dest, src)

      implicit none

      type (average_type), intent(in) :: src
      type (average_type), intent(inout) :: dest

      dest % nAverage % scalar = src % nAverage % scalar
      if (associated(dest % avgSsh % array) .and. associated(src % avgSsh % array)) then
         dest % avgSsh % array = src % avgSsh % array
      end if
      if (associated(dest % varSsh % array) .and. associated(src % varSsh % array)) then
         dest % varSsh % array = src % varSsh % array
      end if
      if (associated(dest % avgVelocityZonal % array) .and. associated(src % avgVelocityZonal % array)) then
         dest % avgVelocityZonal % array = src % avgVelocityZonal % array
      end if
      if (associated(dest % avgVelocityMeridional % array) .and. associated(src % avgVelocityMeridional % array)) then
         dest % avgVelocityMeridional % array = src % avgVelocityMeridional % array
      end if
      if (associated(dest % varVelocityZonal % array) .and. associated(src % varVelocityZonal % array)) then
         dest % varVelocityZonal % array = src % varVelocityZonal % array
      end if
      if (associated(dest % varVelocityMeridional % array) .and. associated(src % varVelocityMeridional % array)) then
         dest % varVelocityMeridional % array = src % varVelocityMeridional % array
      end if
      if (associated(dest % avgNormalVelocity % array) .and. associated(src % avgNormalVelocity % array)) then
         dest % avgNormalVelocity % array = src % avgNormalVelocity % array
      end if
      if (associated(dest % varNormalVelocity % array) .and. associated(src % varNormalVelocity % array)) then
         dest % varNormalVelocity % array = src % varNormalVelocity % array
      end if
      if (associated(dest % avgVertVelocityTop % array) .and. associated(src % avgVertVelocityTop % array)) then
         dest % avgVertVelocityTop % array = src % avgVertVelocityTop % array
      end if

   end subroutine mpas_copy_average


   subroutine mpas_copy_forcing(dest, src)

      implicit none

      type (forcing_type), intent(in) :: src
      type (forcing_type), intent(inout) :: dest

      if (associated(dest % surfaceTracerFlux % array) .and. associated(src % surfaceTracerFlux % array)) then
         dest % surfaceTracerFlux % array = src % surfaceTracerFlux % array
      end if
      if (associated(dest % avgTracersSurfaceValue % array) .and. associated(src % avgTracersSurfaceValue % array)) then
         dest % avgTracersSurfaceValue % array = src % avgTracersSurfaceValue % array
      end if
      if (associated(dest % avgSurfaceVelocity % array) .and. associated(src % avgSurfaceVelocity % array)) then
         dest % avgSurfaceVelocity % array = src % avgSurfaceVelocity % array
      end if
      if (associated(dest % avgSSHGradient % array) .and. associated(src % avgSSHGradient % array)) then
         dest % avgSSHGradient % array = src % avgSSHGradient % array
      end if
      if (associated(dest % surfaceWindStress % array) .and. associated(src % surfaceWindStress % array)) then
         dest % surfaceWindStress % array = src % surfaceWindStress % array
      end if
      if (associated(dest % surfaceWindStressMagnitude % array) .and. associated(src % surfaceWindStressMagnitude % array)) then
         dest % surfaceWindStressMagnitude % array = src % surfaceWindStressMagnitude % array
      end if
      if (associated(dest % surfaceMassFlux % array) .and. associated(src % surfaceMassFlux % array)) then
         dest % surfaceMassFlux % array = src % surfaceMassFlux % array
      end if
      if (associated(dest % seaSurfacePressure % array) .and. associated(src % seaSurfacePressure % array)) then
         dest % seaSurfacePressure % array = src % seaSurfacePressure % array
      end if
      if (associated(dest % seaIceEnergy % array) .and. associated(src % seaIceEnergy % array)) then
         dest % seaIceEnergy % array = src % seaIceEnergy % array
      end if
      if (associated(dest % penetrativeTemperatureFlux % array) .and. associated(src % penetrativeTemperatureFlux % array)) then
         dest % penetrativeTemperatureFlux % array = src % penetrativeTemperatureFlux % array
      end if
      if (associated(dest % transmissionCoefficients % array) .and. associated(src % transmissionCoefficients % array)) then
         dest % transmissionCoefficients % array = src % transmissionCoefficients % array
      end if
      if (associated(dest % windStressZonal % array) .and. associated(src % windStressZonal % array)) then
         dest % windStressZonal % array = src % windStressZonal % array
      end if
      if (associated(dest % windStressMeridional % array) .and. associated(src % windStressMeridional % array)) then
         dest % windStressMeridional % array = src % windStressMeridional % array
      end if
      if (associated(dest % latentHeatFlux % array) .and. associated(src % latentHeatFlux % array)) then
         dest % latentHeatFlux % array = src % latentHeatFlux % array
      end if
      if (associated(dest % sensibleHeatFlux % array) .and. associated(src % sensibleHeatFlux % array)) then
         dest % sensibleHeatFlux % array = src % sensibleHeatFlux % array
      end if
      if (associated(dest % longWaveHeatFluxUp % array) .and. associated(src % longWaveHeatFluxUp % array)) then
         dest % longWaveHeatFluxUp % array = src % longWaveHeatFluxUp % array
      end if
      if (associated(dest % longWaveHeatFluxDown % array) .and. associated(src % longWaveHeatFluxDown % array)) then
         dest % longWaveHeatFluxDown % array = src % longWaveHeatFluxDown % array
      end if
      if (associated(dest % seaIceHeatFlux % array) .and. associated(src % seaIceHeatFlux % array)) then
         dest % seaIceHeatFlux % array = src % seaIceHeatFlux % array
      end if
      if (associated(dest % shortWaveHeatFlux % array) .and. associated(src % shortWaveHeatFlux % array)) then
         dest % shortWaveHeatFlux % array = src % shortWaveHeatFlux % array
      end if
      if (associated(dest % evaporationFlux % array) .and. associated(src % evaporationFlux % array)) then
         dest % evaporationFlux % array = src % evaporationFlux % array
      end if
      if (associated(dest % seaIceSalinityFlux % array) .and. associated(src % seaIceSalinityFlux % array)) then
         dest % seaIceSalinityFlux % array = src % seaIceSalinityFlux % array
      end if
      if (associated(dest % seaIceFreshWaterFlux % array) .and. associated(src % seaIceFreshWaterFlux % array)) then
         dest % seaIceFreshWaterFlux % array = src % seaIceFreshWaterFlux % array
      end if
      if (associated(dest % riverRunoffFlux % array) .and. associated(src % riverRunoffFlux % array)) then
         dest % riverRunoffFlux % array = src % riverRunoffFlux % array
      end if
      if (associated(dest % iceRunoffFlux % array) .and. associated(src % iceRunoffFlux % array)) then
         dest % iceRunoffFlux % array = src % iceRunoffFlux % array
      end if
      if (associated(dest % rainFlux % array) .and. associated(src % rainFlux % array)) then
         dest % rainFlux % array = src % rainFlux % array
      end if
      if (associated(dest % snowFlux % array) .and. associated(src % snowFlux % array)) then
         dest % snowFlux % array = src % snowFlux % array
      end if
      if (associated(dest % iceFraction % array) .and. associated(src % iceFraction % array)) then
         dest % iceFraction % array = src % iceFraction % array
      end if
      if (associated(dest % prognosticCO2 % array) .and. associated(src % prognosticCO2 % array)) then
         dest % prognosticCO2 % array = src % prognosticCO2 % array
      end if
      if (associated(dest % diagnosticCO2 % array) .and. associated(src % diagnosticCO2 % array)) then
         dest % diagnosticCO2 % array = src % diagnosticCO2 % array
      end if
      if (associated(dest % squaredWindSpeed10Meter % array) .and. associated(src % squaredWindSpeed10Meter % array)) then
         dest % squaredWindSpeed10Meter % array = src % squaredWindSpeed10Meter % array
      end if
      dest % nAccumulatedCoupled % scalar = src % nAccumulatedCoupled % scalar
      if (associated(dest % CO2Flux % array) .and. associated(src % CO2Flux % array)) then
         dest % CO2Flux % array = src % CO2Flux % array
      end if
      if (associated(dest % DMSFlux % array) .and. associated(src % DMSFlux % array)) then
         dest % DMSFlux % array = src % DMSFlux % array
      end if

   end subroutine mpas_copy_forcing


   subroutine mpas_copy_scratch(dest, src)

      implicit none

      type (scratch_type), intent(in) :: src
      type (scratch_type), intent(inout) :: dest

      if (associated(dest % vorticityGradientTangentialComponent % array) .and. associated(src % &
vorticityGradientTangentialComponent % array)) then
         dest % vorticityGradientTangentialComponent % array = src % vorticityGradientTangentialComponent % array
      end if
      if (associated(dest % vorticityGradientNormalComponent % array) .and. associated(src % vorticityGradientNormalComponent % &
array)) then
         dest % vorticityGradientNormalComponent % array = src % vorticityGradientNormalComponent % array
      end if
      if (associated(dest % normalizedRelativeVorticityVertex % array) .and. associated(src % normalizedRelativeVorticityVertex % &
array)) then
         dest % normalizedRelativeVorticityVertex % array = src % normalizedRelativeVorticityVertex % array
      end if
      if (associated(dest % normalizedPlanetaryVorticityVertex % array) .and. associated(src % normalizedPlanetaryVorticityVertex &
% array)) then
         dest % normalizedPlanetaryVorticityVertex % array = src % normalizedPlanetaryVorticityVertex % array
      end if
      if (associated(dest % kineticEnergyVertex % array) .and. associated(src % kineticEnergyVertex % array)) then
         dest % kineticEnergyVertex % array = src % kineticEnergyVertex % array
      end if
      if (associated(dest % kineticEnergyVertexOnCells % array) .and. associated(src % kineticEnergyVertexOnCells % array)) then
         dest % kineticEnergyVertexOnCells % array = src % kineticEnergyVertexOnCells % array
      end if
      if (associated(dest % densitySurfaceDisplaced % array) .and. associated(src % densitySurfaceDisplaced % array)) then
         dest % densitySurfaceDisplaced % array = src % densitySurfaceDisplaced % array
      end if
      if (associated(dest % thermalExpansionCoeff % array) .and. associated(src % thermalExpansionCoeff % array)) then
         dest % thermalExpansionCoeff % array = src % thermalExpansionCoeff % array
      end if
      if (associated(dest % salineContractionCoeff % array) .and. associated(src % salineContractionCoeff % array)) then
         dest % salineContractionCoeff % array = src % salineContractionCoeff % array
      end if
      if (associated(dest % normalVelocityTest % array) .and. associated(src % normalVelocityTest % array)) then
         dest % normalVelocityTest % array = src % normalVelocityTest % array
      end if
      if (associated(dest % tangentialVelocityTest % array) .and. associated(src % tangentialVelocityTest % array)) then
         dest % tangentialVelocityTest % array = src % tangentialVelocityTest % array
      end if
      if (associated(dest % strainRateR3Cell % array) .and. associated(src % strainRateR3Cell % array)) then
         dest % strainRateR3Cell % array = src % strainRateR3Cell % array
      end if
      if (associated(dest % strainRateR3CellSolution % array) .and. associated(src % strainRateR3CellSolution % array)) then
         dest % strainRateR3CellSolution % array = src % strainRateR3CellSolution % array
      end if
      if (associated(dest % strainRateR3Edge % array) .and. associated(src % strainRateR3Edge % array)) then
         dest % strainRateR3Edge % array = src % strainRateR3Edge % array
      end if
      if (associated(dest % strainRateLonLatRCell % array) .and. associated(src % strainRateLonLatRCell % array)) then
         dest % strainRateLonLatRCell % array = src % strainRateLonLatRCell % array
      end if
      if (associated(dest % strainRateLonLatRCellSolution % array) .and. associated(src % strainRateLonLatRCellSolution % array)) &
then
         dest % strainRateLonLatRCellSolution % array = src % strainRateLonLatRCellSolution % array
      end if
      if (associated(dest % strainRateLonLatREdge % array) .and. associated(src % strainRateLonLatREdge % array)) then
         dest % strainRateLonLatREdge % array = src % strainRateLonLatREdge % array
      end if
      if (associated(dest % divTensorR3Cell % array) .and. associated(src % divTensorR3Cell % array)) then
         dest % divTensorR3Cell % array = src % divTensorR3Cell % array
      end if
      if (associated(dest % divTensorR3CellSolution % array) .and. associated(src % divTensorR3CellSolution % array)) then
         dest % divTensorR3CellSolution % array = src % divTensorR3CellSolution % array
      end if
      if (associated(dest % divTensorLonLatRCell % array) .and. associated(src % divTensorLonLatRCell % array)) then
         dest % divTensorLonLatRCell % array = src % divTensorLonLatRCell % array
      end if
      if (associated(dest % divTensorLonLatRCellSolution % array) .and. associated(src % divTensorLonLatRCellSolution % array)) &
then
         dest % divTensorLonLatRCellSolution % array = src % divTensorLonLatRCellSolution % array
      end if
      if (associated(dest % outerProductEdge % array) .and. associated(src % outerProductEdge % array)) then
         dest % outerProductEdge % array = src % outerProductEdge % array
      end if
      if (associated(dest % normalVectorEdge % array) .and. associated(src % normalVectorEdge % array)) then
         dest % normalVectorEdge % array = src % normalVectorEdge % array
      end if
      if (associated(dest % tangentialVectorEdge % array) .and. associated(src % tangentialVectorEdge % array)) then
         dest % tangentialVectorEdge % array = src % tangentialVectorEdge % array
      end if
      if (associated(dest % windStressFull % array) .and. associated(src % windStressFull % array)) then
         dest % windStressFull % array = src % windStressFull % array
      end if
      if (associated(dest % windStressX % array) .and. associated(src % windStressX % array)) then
         dest % windStressX % array = src % windStressX % array
      end if
      if (associated(dest % windStressY % array) .and. associated(src % windStressY % array)) then
         dest % windStressY % array = src % windStressY % array
      end if
      if (associated(dest % windStressZ % array) .and. associated(src % windStressZ % array)) then
         dest % windStressZ % array = src % windStressZ % array
      end if
      if (associated(dest % windStressZonal % array) .and. associated(src % windStressZonal % array)) then
         dest % windStressZonal % array = src % windStressZonal % array
      end if
      if (associated(dest % windStressMeridional % array) .and. associated(src % windStressMeridional % array)) then
         dest % windStressMeridional % array = src % windStressMeridional % array
      end if

   end subroutine mpas_copy_scratch




   subroutine mpas_shift_time_levels_state(state)

      implicit none

      type (state_multilevel_type), intent(inout) :: state

      integer :: i
      real (kind=RKIND) :: real0d
      real (kind=RKIND), dimension(:), pointer :: real1d
      real (kind=RKIND), dimension(:,:), pointer :: real2d
      real (kind=RKIND), dimension(:,:,:), pointer :: real3d
      integer :: int0d
      integer, dimension(:), pointer :: int1d
      integer, dimension(:,:), pointer :: int2d
      integer, dimension(:,:,:), pointer :: int3d
      character (len=64) :: char0d
      character (len=64), dimension(:), pointer :: char1d

      real3d => state % time_levs(1) % state % tracers % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % tracers % array => state % time_levs(i+1) % state % tracers % array
      end do
      state % time_levs(state % nTimeLevels) % state % tracers % array=> real3d

      real2d => state % time_levs(1) % state % normalVelocity % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % normalVelocity % array => state % time_levs(i+1) % state % normalVelocity % array
      end do
      state % time_levs(state % nTimeLevels) % state % normalVelocity % array=> real2d

      real2d => state % time_levs(1) % state % layerThickness % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % layerThickness % array => state % time_levs(i+1) % state % layerThickness % array
      end do
      state % time_levs(state % nTimeLevels) % state % layerThickness % array=> real2d

      real1d => state % time_levs(1) % state % ssh % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % ssh % array => state % time_levs(i+1) % state % ssh % array
      end do
      state % time_levs(state % nTimeLevels) % state % ssh % array=> real1d

      real2d => state % time_levs(1) % state % highFreqThickness % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % highFreqThickness % array => state % time_levs(i+1) % state % highFreqThickness % array
      end do
      state % time_levs(state % nTimeLevels) % state % highFreqThickness % array=> real2d

      real2d => state % time_levs(1) % state % lowFreqDivergence % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % lowFreqDivergence % array => state % time_levs(i+1) % state % lowFreqDivergence % array
      end do
      state % time_levs(state % nTimeLevels) % state % lowFreqDivergence % array=> real2d

      real1d => state % time_levs(1) % state % normalBarotropicVelocity % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % normalBarotropicVelocity % array => state % time_levs(i+1) % state % &
normalBarotropicVelocity % array
      end do
      state % time_levs(state % nTimeLevels) % state % normalBarotropicVelocity % array=> real1d

      real1d => state % time_levs(1) % state % normalBarotropicVelocitySubcycle % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % normalBarotropicVelocitySubcycle % array => state % time_levs(i+1) % state % &
normalBarotropicVelocitySubcycle % array
      end do
      state % time_levs(state % nTimeLevels) % state % normalBarotropicVelocitySubcycle % array=> real1d

      real1d => state % time_levs(1) % state % sshSubcycle % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % sshSubcycle % array => state % time_levs(i+1) % state % sshSubcycle % array
      end do
      state % time_levs(state % nTimeLevels) % state % sshSubcycle % array=> real1d

      real2d => state % time_levs(1) % state % normalBaroclinicVelocity % array
      do i=1,state % nTimeLevels-1
         state % time_levs(i) % state % normalBaroclinicVelocity % array => state % time_levs(i+1) % state % &
normalBaroclinicVelocity % array
      end do
      state % time_levs(state % nTimeLevels) % state % normalBaroclinicVelocity % array=> real2d


   end subroutine mpas_shift_time_levels_state




   subroutine mpas_create_field_links(b)

      implicit none
      type (block_type), pointer :: b
      type (block_type), pointer :: prev, next

      if (associated(b % prev)) then
         prev => b % prev
      else
         nullify(prev)
      end if
      if (associated(b % next)) then
         next => b % next
      else
         nullify(next)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_state_links(b % state % time_levs(1) % state,  prev = prev % state % time_levs(1) % state, next = next % &
state % time_levs(1) % state)
      else if (associated(next)) then
         call mpas_create_state_links(b % state % time_levs(1) % state, next = next % state % time_levs(1) % state)
      else if (associated(prev)) then
         call mpas_create_state_links(b % state % time_levs(1) % state, prev = prev % state % time_levs(1) % state)
      else
         call mpas_create_state_links(b % state % time_levs(1) % state)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_state_links(b % state % time_levs(2) % state,  prev = prev % state % time_levs(2) % state, next = next % &
state % time_levs(2) % state)
      else if (associated(next)) then
         call mpas_create_state_links(b % state % time_levs(2) % state, next = next % state % time_levs(2) % state)
      else if (associated(prev)) then
         call mpas_create_state_links(b % state % time_levs(2) % state, prev = prev % state % time_levs(2) % state)
      else
         call mpas_create_state_links(b % state % time_levs(2) % state)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_mesh_links(b % mesh, prev = prev % mesh, next = next % mesh)
      else if (associated(next)) then
         call mpas_create_mesh_links(b % mesh, next = next % mesh)
      else if (associated(prev)) then
         call mpas_create_mesh_links(b % mesh, prev = prev % mesh)
      else
         call mpas_create_mesh_links(b % mesh)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_verticalMesh_links(b % verticalMesh, prev = prev % verticalMesh, next = next % verticalMesh)
      else if (associated(next)) then
         call mpas_create_verticalMesh_links(b % verticalMesh, next = next % verticalMesh)
      else if (associated(prev)) then
         call mpas_create_verticalMesh_links(b % verticalMesh, prev = prev % verticalMesh)
      else
         call mpas_create_verticalMesh_links(b % verticalMesh)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_tend_links(b % tend, prev = prev % tend, next = next % tend)
      else if (associated(next)) then
         call mpas_create_tend_links(b % tend, next = next % tend)
      else if (associated(prev)) then
         call mpas_create_tend_links(b % tend, prev = prev % tend)
      else
         call mpas_create_tend_links(b % tend)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_diagnostics_links(b % diagnostics, prev = prev % diagnostics, next = next % diagnostics)
      else if (associated(next)) then
         call mpas_create_diagnostics_links(b % diagnostics, next = next % diagnostics)
      else if (associated(prev)) then
         call mpas_create_diagnostics_links(b % diagnostics, prev = prev % diagnostics)
      else
         call mpas_create_diagnostics_links(b % diagnostics)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_average_links(b % average, prev = prev % average, next = next % average)
      else if (associated(next)) then
         call mpas_create_average_links(b % average, next = next % average)
      else if (associated(prev)) then
         call mpas_create_average_links(b % average, prev = prev % average)
      else
         call mpas_create_average_links(b % average)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_forcing_links(b % forcing, prev = prev % forcing, next = next % forcing)
      else if (associated(next)) then
         call mpas_create_forcing_links(b % forcing, next = next % forcing)
      else if (associated(prev)) then
         call mpas_create_forcing_links(b % forcing, prev = prev % forcing)
      else
         call mpas_create_forcing_links(b % forcing)
      end if

      if (associated(next) .and. associated(prev)) then
         call mpas_create_scratch_links(b % scratch, prev = prev % scratch, next = next % scratch)
      else if (associated(next)) then
         call mpas_create_scratch_links(b % scratch, next = next % scratch)
      else if (associated(prev)) then
         call mpas_create_scratch_links(b % scratch, prev = prev % scratch)
      else
         call mpas_create_scratch_links(b % scratch)
      end if

   end subroutine mpas_create_field_links


      subroutine mpas_create_state_links(state, prev, next)

         implicit none
         type (state_type), pointer :: state
         type (state_type), pointer, optional :: prev, next
         state % tracers % sendList => state % tracers % block % parinfo % cellsToSend
         state % tracers % recvList => state % tracers % block % parinfo % cellsToRecv
         state % tracers % copyList => state % tracers % block % parinfo % cellsToCopy
         if(present(prev)) then
           state % tracers % prev => prev % tracers
         else
           nullify(state % tracers % prev)
         end if
         if(present(next)) then
           state % tracers % next => next % tracers
         else
           nullify(state % tracers % next)
         end if


         state % normalVelocity % sendList => state % normalVelocity % block % parinfo % edgesToSend
         state % normalVelocity % recvList => state % normalVelocity % block % parinfo % edgesToRecv
         state % normalVelocity % copyList => state % normalVelocity % block % parinfo % edgesToCopy
         if(present(prev)) then
           state % normalVelocity % prev => prev % normalVelocity
         else
           nullify(state % normalVelocity % prev)
         end if
         if(present(next)) then
           state % normalVelocity % next => next % normalVelocity
         else
           nullify(state % normalVelocity % next)
         end if


         state % layerThickness % sendList => state % layerThickness % block % parinfo % cellsToSend
         state % layerThickness % recvList => state % layerThickness % block % parinfo % cellsToRecv
         state % layerThickness % copyList => state % layerThickness % block % parinfo % cellsToCopy
         if(present(prev)) then
           state % layerThickness % prev => prev % layerThickness
         else
           nullify(state % layerThickness % prev)
         end if
         if(present(next)) then
           state % layerThickness % next => next % layerThickness
         else
           nullify(state % layerThickness % next)
         end if


         state % ssh % sendList => state % ssh % block % parinfo % cellsToSend
         state % ssh % recvList => state % ssh % block % parinfo % cellsToRecv
         state % ssh % copyList => state % ssh % block % parinfo % cellsToCopy
         if(present(prev)) then
           state % ssh % prev => prev % ssh
         else
           nullify(state % ssh % prev)
         end if
         if(present(next)) then
           state % ssh % next => next % ssh
         else
           nullify(state % ssh % next)
         end if


         state % highFreqThickness % sendList => state % highFreqThickness % block % parinfo % cellsToSend
         state % highFreqThickness % recvList => state % highFreqThickness % block % parinfo % cellsToRecv
         state % highFreqThickness % copyList => state % highFreqThickness % block % parinfo % cellsToCopy
         if(present(prev)) then
           state % highFreqThickness % prev => prev % highFreqThickness
         else
           nullify(state % highFreqThickness % prev)
         end if
         if(present(next)) then
           state % highFreqThickness % next => next % highFreqThickness
         else
           nullify(state % highFreqThickness % next)
         end if


         state % lowFreqDivergence % sendList => state % lowFreqDivergence % block % parinfo % cellsToSend
         state % lowFreqDivergence % recvList => state % lowFreqDivergence % block % parinfo % cellsToRecv
         state % lowFreqDivergence % copyList => state % lowFreqDivergence % block % parinfo % cellsToCopy
         if(present(prev)) then
           state % lowFreqDivergence % prev => prev % lowFreqDivergence
         else
           nullify(state % lowFreqDivergence % prev)
         end if
         if(present(next)) then
           state % lowFreqDivergence % next => next % lowFreqDivergence
         else
           nullify(state % lowFreqDivergence % next)
         end if


         state % normalBarotropicVelocity % sendList => state % normalBarotropicVelocity % block % parinfo % edgesToSend
         state % normalBarotropicVelocity % recvList => state % normalBarotropicVelocity % block % parinfo % edgesToRecv
         state % normalBarotropicVelocity % copyList => state % normalBarotropicVelocity % block % parinfo % edgesToCopy
         if(present(prev)) then
           state % normalBarotropicVelocity % prev => prev % normalBarotropicVelocity
         else
           nullify(state % normalBarotropicVelocity % prev)
         end if
         if(present(next)) then
           state % normalBarotropicVelocity % next => next % normalBarotropicVelocity
         else
           nullify(state % normalBarotropicVelocity % next)
         end if


         state % normalBarotropicVelocitySubcycle % sendList => state % normalBarotropicVelocitySubcycle % block % parinfo % &
edgesToSend
         state % normalBarotropicVelocitySubcycle % recvList => state % normalBarotropicVelocitySubcycle % block % parinfo % &
edgesToRecv
         state % normalBarotropicVelocitySubcycle % copyList => state % normalBarotropicVelocitySubcycle % block % parinfo % &
edgesToCopy
         if(present(prev)) then
           state % normalBarotropicVelocitySubcycle % prev => prev % normalBarotropicVelocitySubcycle
         else
           nullify(state % normalBarotropicVelocitySubcycle % prev)
         end if
         if(present(next)) then
           state % normalBarotropicVelocitySubcycle % next => next % normalBarotropicVelocitySubcycle
         else
           nullify(state % normalBarotropicVelocitySubcycle % next)
         end if


         state % sshSubcycle % sendList => state % sshSubcycle % block % parinfo % cellsToSend
         state % sshSubcycle % recvList => state % sshSubcycle % block % parinfo % cellsToRecv
         state % sshSubcycle % copyList => state % sshSubcycle % block % parinfo % cellsToCopy
         if(present(prev)) then
           state % sshSubcycle % prev => prev % sshSubcycle
         else
           nullify(state % sshSubcycle % prev)
         end if
         if(present(next)) then
           state % sshSubcycle % next => next % sshSubcycle
         else
           nullify(state % sshSubcycle % next)
         end if


         state % normalBaroclinicVelocity % sendList => state % normalBaroclinicVelocity % block % parinfo % edgesToSend
         state % normalBaroclinicVelocity % recvList => state % normalBaroclinicVelocity % block % parinfo % edgesToRecv
         state % normalBaroclinicVelocity % copyList => state % normalBaroclinicVelocity % block % parinfo % edgesToCopy
         if(present(prev)) then
           state % normalBaroclinicVelocity % prev => prev % normalBaroclinicVelocity
         else
           nullify(state % normalBaroclinicVelocity % prev)
         end if
         if(present(next)) then
           state % normalBaroclinicVelocity % next => next % normalBaroclinicVelocity
         else
           nullify(state % normalBaroclinicVelocity % next)
         end if


      end subroutine mpas_create_state_links


      subroutine mpas_create_mesh_links(mesh, prev, next)

         implicit none
         type (mesh_type), pointer :: mesh
         type (mesh_type), pointer, optional :: prev, next
         mesh % latCell % sendList => mesh % latCell % block % parinfo % cellsToSend
         mesh % latCell % recvList => mesh % latCell % block % parinfo % cellsToRecv
         mesh % latCell % copyList => mesh % latCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % latCell % prev => prev % latCell
         else
           nullify(mesh % latCell % prev)
         end if
         if(present(next)) then
           mesh % latCell % next => next % latCell
         else
           nullify(mesh % latCell % next)
         end if


         mesh % lonCell % sendList => mesh % lonCell % block % parinfo % cellsToSend
         mesh % lonCell % recvList => mesh % lonCell % block % parinfo % cellsToRecv
         mesh % lonCell % copyList => mesh % lonCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % lonCell % prev => prev % lonCell
         else
           nullify(mesh % lonCell % prev)
         end if
         if(present(next)) then
           mesh % lonCell % next => next % lonCell
         else
           nullify(mesh % lonCell % next)
         end if


         mesh % xCell % sendList => mesh % xCell % block % parinfo % cellsToSend
         mesh % xCell % recvList => mesh % xCell % block % parinfo % cellsToRecv
         mesh % xCell % copyList => mesh % xCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % xCell % prev => prev % xCell
         else
           nullify(mesh % xCell % prev)
         end if
         if(present(next)) then
           mesh % xCell % next => next % xCell
         else
           nullify(mesh % xCell % next)
         end if


         mesh % yCell % sendList => mesh % yCell % block % parinfo % cellsToSend
         mesh % yCell % recvList => mesh % yCell % block % parinfo % cellsToRecv
         mesh % yCell % copyList => mesh % yCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % yCell % prev => prev % yCell
         else
           nullify(mesh % yCell % prev)
         end if
         if(present(next)) then
           mesh % yCell % next => next % yCell
         else
           nullify(mesh % yCell % next)
         end if


         mesh % zCell % sendList => mesh % zCell % block % parinfo % cellsToSend
         mesh % zCell % recvList => mesh % zCell % block % parinfo % cellsToRecv
         mesh % zCell % copyList => mesh % zCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % zCell % prev => prev % zCell
         else
           nullify(mesh % zCell % prev)
         end if
         if(present(next)) then
           mesh % zCell % next => next % zCell
         else
           nullify(mesh % zCell % next)
         end if


         mesh % indexToCellID % sendList => mesh % indexToCellID % block % parinfo % cellsToSend
         mesh % indexToCellID % recvList => mesh % indexToCellID % block % parinfo % cellsToRecv
         mesh % indexToCellID % copyList => mesh % indexToCellID % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % indexToCellID % prev => prev % indexToCellID
         else
           nullify(mesh % indexToCellID % prev)
         end if
         if(present(next)) then
           mesh % indexToCellID % next => next % indexToCellID
         else
           nullify(mesh % indexToCellID % next)
         end if


         mesh % latEdge % sendList => mesh % latEdge % block % parinfo % edgesToSend
         mesh % latEdge % recvList => mesh % latEdge % block % parinfo % edgesToRecv
         mesh % latEdge % copyList => mesh % latEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % latEdge % prev => prev % latEdge
         else
           nullify(mesh % latEdge % prev)
         end if
         if(present(next)) then
           mesh % latEdge % next => next % latEdge
         else
           nullify(mesh % latEdge % next)
         end if


         mesh % lonEdge % sendList => mesh % lonEdge % block % parinfo % edgesToSend
         mesh % lonEdge % recvList => mesh % lonEdge % block % parinfo % edgesToRecv
         mesh % lonEdge % copyList => mesh % lonEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % lonEdge % prev => prev % lonEdge
         else
           nullify(mesh % lonEdge % prev)
         end if
         if(present(next)) then
           mesh % lonEdge % next => next % lonEdge
         else
           nullify(mesh % lonEdge % next)
         end if


         mesh % xEdge % sendList => mesh % xEdge % block % parinfo % edgesToSend
         mesh % xEdge % recvList => mesh % xEdge % block % parinfo % edgesToRecv
         mesh % xEdge % copyList => mesh % xEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % xEdge % prev => prev % xEdge
         else
           nullify(mesh % xEdge % prev)
         end if
         if(present(next)) then
           mesh % xEdge % next => next % xEdge
         else
           nullify(mesh % xEdge % next)
         end if


         mesh % yEdge % sendList => mesh % yEdge % block % parinfo % edgesToSend
         mesh % yEdge % recvList => mesh % yEdge % block % parinfo % edgesToRecv
         mesh % yEdge % copyList => mesh % yEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % yEdge % prev => prev % yEdge
         else
           nullify(mesh % yEdge % prev)
         end if
         if(present(next)) then
           mesh % yEdge % next => next % yEdge
         else
           nullify(mesh % yEdge % next)
         end if


         mesh % zEdge % sendList => mesh % zEdge % block % parinfo % edgesToSend
         mesh % zEdge % recvList => mesh % zEdge % block % parinfo % edgesToRecv
         mesh % zEdge % copyList => mesh % zEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % zEdge % prev => prev % zEdge
         else
           nullify(mesh % zEdge % prev)
         end if
         if(present(next)) then
           mesh % zEdge % next => next % zEdge
         else
           nullify(mesh % zEdge % next)
         end if


         mesh % indexToEdgeID % sendList => mesh % indexToEdgeID % block % parinfo % edgesToSend
         mesh % indexToEdgeID % recvList => mesh % indexToEdgeID % block % parinfo % edgesToRecv
         mesh % indexToEdgeID % copyList => mesh % indexToEdgeID % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % indexToEdgeID % prev => prev % indexToEdgeID
         else
           nullify(mesh % indexToEdgeID % prev)
         end if
         if(present(next)) then
           mesh % indexToEdgeID % next => next % indexToEdgeID
         else
           nullify(mesh % indexToEdgeID % next)
         end if


         mesh % latVertex % sendList => mesh % latVertex % block % parinfo % verticesToSend
         mesh % latVertex % recvList => mesh % latVertex % block % parinfo % verticesToRecv
         mesh % latVertex % copyList => mesh % latVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % latVertex % prev => prev % latVertex
         else
           nullify(mesh % latVertex % prev)
         end if
         if(present(next)) then
           mesh % latVertex % next => next % latVertex
         else
           nullify(mesh % latVertex % next)
         end if


         mesh % lonVertex % sendList => mesh % lonVertex % block % parinfo % verticesToSend
         mesh % lonVertex % recvList => mesh % lonVertex % block % parinfo % verticesToRecv
         mesh % lonVertex % copyList => mesh % lonVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % lonVertex % prev => prev % lonVertex
         else
           nullify(mesh % lonVertex % prev)
         end if
         if(present(next)) then
           mesh % lonVertex % next => next % lonVertex
         else
           nullify(mesh % lonVertex % next)
         end if


         mesh % xVertex % sendList => mesh % xVertex % block % parinfo % verticesToSend
         mesh % xVertex % recvList => mesh % xVertex % block % parinfo % verticesToRecv
         mesh % xVertex % copyList => mesh % xVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % xVertex % prev => prev % xVertex
         else
           nullify(mesh % xVertex % prev)
         end if
         if(present(next)) then
           mesh % xVertex % next => next % xVertex
         else
           nullify(mesh % xVertex % next)
         end if


         mesh % yVertex % sendList => mesh % yVertex % block % parinfo % verticesToSend
         mesh % yVertex % recvList => mesh % yVertex % block % parinfo % verticesToRecv
         mesh % yVertex % copyList => mesh % yVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % yVertex % prev => prev % yVertex
         else
           nullify(mesh % yVertex % prev)
         end if
         if(present(next)) then
           mesh % yVertex % next => next % yVertex
         else
           nullify(mesh % yVertex % next)
         end if


         mesh % zVertex % sendList => mesh % zVertex % block % parinfo % verticesToSend
         mesh % zVertex % recvList => mesh % zVertex % block % parinfo % verticesToRecv
         mesh % zVertex % copyList => mesh % zVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % zVertex % prev => prev % zVertex
         else
           nullify(mesh % zVertex % prev)
         end if
         if(present(next)) then
           mesh % zVertex % next => next % zVertex
         else
           nullify(mesh % zVertex % next)
         end if


         mesh % indexToVertexID % sendList => mesh % indexToVertexID % block % parinfo % verticesToSend
         mesh % indexToVertexID % recvList => mesh % indexToVertexID % block % parinfo % verticesToRecv
         mesh % indexToVertexID % copyList => mesh % indexToVertexID % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % indexToVertexID % prev => prev % indexToVertexID
         else
           nullify(mesh % indexToVertexID % prev)
         end if
         if(present(next)) then
           mesh % indexToVertexID % next => next % indexToVertexID
         else
           nullify(mesh % indexToVertexID % next)
         end if


         mesh % meshDensity % sendList => mesh % meshDensity % block % parinfo % cellsToSend
         mesh % meshDensity % recvList => mesh % meshDensity % block % parinfo % cellsToRecv
         mesh % meshDensity % copyList => mesh % meshDensity % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % meshDensity % prev => prev % meshDensity
         else
           nullify(mesh % meshDensity % prev)
         end if
         if(present(next)) then
           mesh % meshDensity % next => next % meshDensity
         else
           nullify(mesh % meshDensity % next)
         end if


         mesh % meshScalingDel2 % sendList => mesh % meshScalingDel2 % block % parinfo % edgesToSend
         mesh % meshScalingDel2 % recvList => mesh % meshScalingDel2 % block % parinfo % edgesToRecv
         mesh % meshScalingDel2 % copyList => mesh % meshScalingDel2 % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % meshScalingDel2 % prev => prev % meshScalingDel2
         else
           nullify(mesh % meshScalingDel2 % prev)
         end if
         if(present(next)) then
           mesh % meshScalingDel2 % next => next % meshScalingDel2
         else
           nullify(mesh % meshScalingDel2 % next)
         end if


         mesh % meshScalingDel4 % sendList => mesh % meshScalingDel4 % block % parinfo % edgesToSend
         mesh % meshScalingDel4 % recvList => mesh % meshScalingDel4 % block % parinfo % edgesToRecv
         mesh % meshScalingDel4 % copyList => mesh % meshScalingDel4 % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % meshScalingDel4 % prev => prev % meshScalingDel4
         else
           nullify(mesh % meshScalingDel4 % prev)
         end if
         if(present(next)) then
           mesh % meshScalingDel4 % next => next % meshScalingDel4
         else
           nullify(mesh % meshScalingDel4 % next)
         end if


         mesh % meshScaling % sendList => mesh % meshScaling % block % parinfo % edgesToSend
         mesh % meshScaling % recvList => mesh % meshScaling % block % parinfo % edgesToRecv
         mesh % meshScaling % copyList => mesh % meshScaling % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % meshScaling % prev => prev % meshScaling
         else
           nullify(mesh % meshScaling % prev)
         end if
         if(present(next)) then
           mesh % meshScaling % next => next % meshScaling
         else
           nullify(mesh % meshScaling % next)
         end if


         mesh % cellsOnEdge % sendList => mesh % cellsOnEdge % block % parinfo % edgesToSend
         mesh % cellsOnEdge % recvList => mesh % cellsOnEdge % block % parinfo % edgesToRecv
         mesh % cellsOnEdge % copyList => mesh % cellsOnEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % cellsOnEdge % prev => prev % cellsOnEdge
         else
           nullify(mesh % cellsOnEdge % prev)
         end if
         if(present(next)) then
           mesh % cellsOnEdge % next => next % cellsOnEdge
         else
           nullify(mesh % cellsOnEdge % next)
         end if


         mesh % nEdgesOnCell % sendList => mesh % nEdgesOnCell % block % parinfo % cellsToSend
         mesh % nEdgesOnCell % recvList => mesh % nEdgesOnCell % block % parinfo % cellsToRecv
         mesh % nEdgesOnCell % copyList => mesh % nEdgesOnCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % nEdgesOnCell % prev => prev % nEdgesOnCell
         else
           nullify(mesh % nEdgesOnCell % prev)
         end if
         if(present(next)) then
           mesh % nEdgesOnCell % next => next % nEdgesOnCell
         else
           nullify(mesh % nEdgesOnCell % next)
         end if


         mesh % nEdgesOnEdge % sendList => mesh % nEdgesOnEdge % block % parinfo % edgesToSend
         mesh % nEdgesOnEdge % recvList => mesh % nEdgesOnEdge % block % parinfo % edgesToRecv
         mesh % nEdgesOnEdge % copyList => mesh % nEdgesOnEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % nEdgesOnEdge % prev => prev % nEdgesOnEdge
         else
           nullify(mesh % nEdgesOnEdge % prev)
         end if
         if(present(next)) then
           mesh % nEdgesOnEdge % next => next % nEdgesOnEdge
         else
           nullify(mesh % nEdgesOnEdge % next)
         end if


         mesh % edgesOnCell % sendList => mesh % edgesOnCell % block % parinfo % cellsToSend
         mesh % edgesOnCell % recvList => mesh % edgesOnCell % block % parinfo % cellsToRecv
         mesh % edgesOnCell % copyList => mesh % edgesOnCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % edgesOnCell % prev => prev % edgesOnCell
         else
           nullify(mesh % edgesOnCell % prev)
         end if
         if(present(next)) then
           mesh % edgesOnCell % next => next % edgesOnCell
         else
           nullify(mesh % edgesOnCell % next)
         end if


         mesh % edgesOnEdge % sendList => mesh % edgesOnEdge % block % parinfo % edgesToSend
         mesh % edgesOnEdge % recvList => mesh % edgesOnEdge % block % parinfo % edgesToRecv
         mesh % edgesOnEdge % copyList => mesh % edgesOnEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % edgesOnEdge % prev => prev % edgesOnEdge
         else
           nullify(mesh % edgesOnEdge % prev)
         end if
         if(present(next)) then
           mesh % edgesOnEdge % next => next % edgesOnEdge
         else
           nullify(mesh % edgesOnEdge % next)
         end if


         mesh % weightsOnEdge % sendList => mesh % weightsOnEdge % block % parinfo % edgesToSend
         mesh % weightsOnEdge % recvList => mesh % weightsOnEdge % block % parinfo % edgesToRecv
         mesh % weightsOnEdge % copyList => mesh % weightsOnEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % weightsOnEdge % prev => prev % weightsOnEdge
         else
           nullify(mesh % weightsOnEdge % prev)
         end if
         if(present(next)) then
           mesh % weightsOnEdge % next => next % weightsOnEdge
         else
           nullify(mesh % weightsOnEdge % next)
         end if


         mesh % dvEdge % sendList => mesh % dvEdge % block % parinfo % edgesToSend
         mesh % dvEdge % recvList => mesh % dvEdge % block % parinfo % edgesToRecv
         mesh % dvEdge % copyList => mesh % dvEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % dvEdge % prev => prev % dvEdge
         else
           nullify(mesh % dvEdge % prev)
         end if
         if(present(next)) then
           mesh % dvEdge % next => next % dvEdge
         else
           nullify(mesh % dvEdge % next)
         end if


         mesh % dcEdge % sendList => mesh % dcEdge % block % parinfo % edgesToSend
         mesh % dcEdge % recvList => mesh % dcEdge % block % parinfo % edgesToRecv
         mesh % dcEdge % copyList => mesh % dcEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % dcEdge % prev => prev % dcEdge
         else
           nullify(mesh % dcEdge % prev)
         end if
         if(present(next)) then
           mesh % dcEdge % next => next % dcEdge
         else
           nullify(mesh % dcEdge % next)
         end if


         mesh % angleEdge % sendList => mesh % angleEdge % block % parinfo % edgesToSend
         mesh % angleEdge % recvList => mesh % angleEdge % block % parinfo % edgesToRecv
         mesh % angleEdge % copyList => mesh % angleEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % angleEdge % prev => prev % angleEdge
         else
           nullify(mesh % angleEdge % prev)
         end if
         if(present(next)) then
           mesh % angleEdge % next => next % angleEdge
         else
           nullify(mesh % angleEdge % next)
         end if


         mesh % areaCell % sendList => mesh % areaCell % block % parinfo % cellsToSend
         mesh % areaCell % recvList => mesh % areaCell % block % parinfo % cellsToRecv
         mesh % areaCell % copyList => mesh % areaCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % areaCell % prev => prev % areaCell
         else
           nullify(mesh % areaCell % prev)
         end if
         if(present(next)) then
           mesh % areaCell % next => next % areaCell
         else
           nullify(mesh % areaCell % next)
         end if


         mesh % areaTriangle % sendList => mesh % areaTriangle % block % parinfo % verticesToSend
         mesh % areaTriangle % recvList => mesh % areaTriangle % block % parinfo % verticesToRecv
         mesh % areaTriangle % copyList => mesh % areaTriangle % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % areaTriangle % prev => prev % areaTriangle
         else
           nullify(mesh % areaTriangle % prev)
         end if
         if(present(next)) then
           mesh % areaTriangle % next => next % areaTriangle
         else
           nullify(mesh % areaTriangle % next)
         end if


         mesh % edgeNormalVectors % sendList => mesh % edgeNormalVectors % block % parinfo % edgesToSend
         mesh % edgeNormalVectors % recvList => mesh % edgeNormalVectors % block % parinfo % edgesToRecv
         mesh % edgeNormalVectors % copyList => mesh % edgeNormalVectors % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % edgeNormalVectors % prev => prev % edgeNormalVectors
         else
           nullify(mesh % edgeNormalVectors % prev)
         end if
         if(present(next)) then
           mesh % edgeNormalVectors % next => next % edgeNormalVectors
         else
           nullify(mesh % edgeNormalVectors % next)
         end if


         mesh % edgeTangentVectors % sendList => mesh % edgeTangentVectors % block % parinfo % edgesToSend
         mesh % edgeTangentVectors % recvList => mesh % edgeTangentVectors % block % parinfo % edgesToRecv
         mesh % edgeTangentVectors % copyList => mesh % edgeTangentVectors % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % edgeTangentVectors % prev => prev % edgeTangentVectors
         else
           nullify(mesh % edgeTangentVectors % prev)
         end if
         if(present(next)) then
           mesh % edgeTangentVectors % next => next % edgeTangentVectors
         else
           nullify(mesh % edgeTangentVectors % next)
         end if


         mesh % localVerticalUnitVectors % sendList => mesh % localVerticalUnitVectors % block % parinfo % cellsToSend
         mesh % localVerticalUnitVectors % recvList => mesh % localVerticalUnitVectors % block % parinfo % cellsToRecv
         mesh % localVerticalUnitVectors % copyList => mesh % localVerticalUnitVectors % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % localVerticalUnitVectors % prev => prev % localVerticalUnitVectors
         else
           nullify(mesh % localVerticalUnitVectors % prev)
         end if
         if(present(next)) then
           mesh % localVerticalUnitVectors % next => next % localVerticalUnitVectors
         else
           nullify(mesh % localVerticalUnitVectors % next)
         end if


         mesh % cellTangentPlane % sendList => mesh % cellTangentPlane % block % parinfo % cellsToSend
         mesh % cellTangentPlane % recvList => mesh % cellTangentPlane % block % parinfo % cellsToRecv
         mesh % cellTangentPlane % copyList => mesh % cellTangentPlane % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % cellTangentPlane % prev => prev % cellTangentPlane
         else
           nullify(mesh % cellTangentPlane % prev)
         end if
         if(present(next)) then
           mesh % cellTangentPlane % next => next % cellTangentPlane
         else
           nullify(mesh % cellTangentPlane % next)
         end if


         mesh % cellsOnCell % sendList => mesh % cellsOnCell % block % parinfo % cellsToSend
         mesh % cellsOnCell % recvList => mesh % cellsOnCell % block % parinfo % cellsToRecv
         mesh % cellsOnCell % copyList => mesh % cellsOnCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % cellsOnCell % prev => prev % cellsOnCell
         else
           nullify(mesh % cellsOnCell % prev)
         end if
         if(present(next)) then
           mesh % cellsOnCell % next => next % cellsOnCell
         else
           nullify(mesh % cellsOnCell % next)
         end if


         mesh % verticesOnCell % sendList => mesh % verticesOnCell % block % parinfo % cellsToSend
         mesh % verticesOnCell % recvList => mesh % verticesOnCell % block % parinfo % cellsToRecv
         mesh % verticesOnCell % copyList => mesh % verticesOnCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % verticesOnCell % prev => prev % verticesOnCell
         else
           nullify(mesh % verticesOnCell % prev)
         end if
         if(present(next)) then
           mesh % verticesOnCell % next => next % verticesOnCell
         else
           nullify(mesh % verticesOnCell % next)
         end if


         mesh % verticesOnEdge % sendList => mesh % verticesOnEdge % block % parinfo % edgesToSend
         mesh % verticesOnEdge % recvList => mesh % verticesOnEdge % block % parinfo % edgesToRecv
         mesh % verticesOnEdge % copyList => mesh % verticesOnEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % verticesOnEdge % prev => prev % verticesOnEdge
         else
           nullify(mesh % verticesOnEdge % prev)
         end if
         if(present(next)) then
           mesh % verticesOnEdge % next => next % verticesOnEdge
         else
           nullify(mesh % verticesOnEdge % next)
         end if


         mesh % edgesOnVertex % sendList => mesh % edgesOnVertex % block % parinfo % verticesToSend
         mesh % edgesOnVertex % recvList => mesh % edgesOnVertex % block % parinfo % verticesToRecv
         mesh % edgesOnVertex % copyList => mesh % edgesOnVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % edgesOnVertex % prev => prev % edgesOnVertex
         else
           nullify(mesh % edgesOnVertex % prev)
         end if
         if(present(next)) then
           mesh % edgesOnVertex % next => next % edgesOnVertex
         else
           nullify(mesh % edgesOnVertex % next)
         end if


         mesh % cellsOnVertex % sendList => mesh % cellsOnVertex % block % parinfo % verticesToSend
         mesh % cellsOnVertex % recvList => mesh % cellsOnVertex % block % parinfo % verticesToRecv
         mesh % cellsOnVertex % copyList => mesh % cellsOnVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % cellsOnVertex % prev => prev % cellsOnVertex
         else
           nullify(mesh % cellsOnVertex % prev)
         end if
         if(present(next)) then
           mesh % cellsOnVertex % next => next % cellsOnVertex
         else
           nullify(mesh % cellsOnVertex % next)
         end if


         mesh % kiteAreasOnVertex % sendList => mesh % kiteAreasOnVertex % block % parinfo % verticesToSend
         mesh % kiteAreasOnVertex % recvList => mesh % kiteAreasOnVertex % block % parinfo % verticesToRecv
         mesh % kiteAreasOnVertex % copyList => mesh % kiteAreasOnVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % kiteAreasOnVertex % prev => prev % kiteAreasOnVertex
         else
           nullify(mesh % kiteAreasOnVertex % prev)
         end if
         if(present(next)) then
           mesh % kiteAreasOnVertex % next => next % kiteAreasOnVertex
         else
           nullify(mesh % kiteAreasOnVertex % next)
         end if


         mesh % fEdge % sendList => mesh % fEdge % block % parinfo % edgesToSend
         mesh % fEdge % recvList => mesh % fEdge % block % parinfo % edgesToRecv
         mesh % fEdge % copyList => mesh % fEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % fEdge % prev => prev % fEdge
         else
           nullify(mesh % fEdge % prev)
         end if
         if(present(next)) then
           mesh % fEdge % next => next % fEdge
         else
           nullify(mesh % fEdge % next)
         end if


         mesh % fVertex % sendList => mesh % fVertex % block % parinfo % verticesToSend
         mesh % fVertex % recvList => mesh % fVertex % block % parinfo % verticesToRecv
         mesh % fVertex % copyList => mesh % fVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % fVertex % prev => prev % fVertex
         else
           nullify(mesh % fVertex % prev)
         end if
         if(present(next)) then
           mesh % fVertex % next => next % fVertex
         else
           nullify(mesh % fVertex % next)
         end if


         mesh % fCell % sendList => mesh % fCell % block % parinfo % cellsToSend
         mesh % fCell % recvList => mesh % fCell % block % parinfo % cellsToRecv
         mesh % fCell % copyList => mesh % fCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % fCell % prev => prev % fCell
         else
           nullify(mesh % fCell % prev)
         end if
         if(present(next)) then
           mesh % fCell % next => next % fCell
         else
           nullify(mesh % fCell % next)
         end if


         mesh % bottomDepth % sendList => mesh % bottomDepth % block % parinfo % cellsToSend
         mesh % bottomDepth % recvList => mesh % bottomDepth % block % parinfo % cellsToRecv
         mesh % bottomDepth % copyList => mesh % bottomDepth % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % bottomDepth % prev => prev % bottomDepth
         else
           nullify(mesh % bottomDepth % prev)
         end if
         if(present(next)) then
           mesh % bottomDepth % next => next % bottomDepth
         else
           nullify(mesh % bottomDepth % next)
         end if


         mesh % derivTwo % sendList => mesh % derivTwo % block % parinfo % edgesToSend
         mesh % derivTwo % recvList => mesh % derivTwo % block % parinfo % edgesToRecv
         mesh % derivTwo % copyList => mesh % derivTwo % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % derivTwo % prev => prev % derivTwo
         else
           nullify(mesh % derivTwo % prev)
         end if
         if(present(next)) then
           mesh % derivTwo % next => next % derivTwo
         else
           nullify(mesh % derivTwo % next)
         end if


         mesh % advCoefs % sendList => mesh % advCoefs % block % parinfo % edgesToSend
         mesh % advCoefs % recvList => mesh % advCoefs % block % parinfo % edgesToRecv
         mesh % advCoefs % copyList => mesh % advCoefs % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % advCoefs % prev => prev % advCoefs
         else
           nullify(mesh % advCoefs % prev)
         end if
         if(present(next)) then
           mesh % advCoefs % next => next % advCoefs
         else
           nullify(mesh % advCoefs % next)
         end if


         mesh % advCoefs3rd % sendList => mesh % advCoefs3rd % block % parinfo % edgesToSend
         mesh % advCoefs3rd % recvList => mesh % advCoefs3rd % block % parinfo % edgesToRecv
         mesh % advCoefs3rd % copyList => mesh % advCoefs3rd % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % advCoefs3rd % prev => prev % advCoefs3rd
         else
           nullify(mesh % advCoefs3rd % prev)
         end if
         if(present(next)) then
           mesh % advCoefs3rd % next => next % advCoefs3rd
         else
           nullify(mesh % advCoefs3rd % next)
         end if


         mesh % advCellsForEdge % sendList => mesh % advCellsForEdge % block % parinfo % edgesToSend
         mesh % advCellsForEdge % recvList => mesh % advCellsForEdge % block % parinfo % edgesToRecv
         mesh % advCellsForEdge % copyList => mesh % advCellsForEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % advCellsForEdge % prev => prev % advCellsForEdge
         else
           nullify(mesh % advCellsForEdge % prev)
         end if
         if(present(next)) then
           mesh % advCellsForEdge % next => next % advCellsForEdge
         else
           nullify(mesh % advCellsForEdge % next)
         end if


         mesh % nAdvCellsForEdge % sendList => mesh % nAdvCellsForEdge % block % parinfo % edgesToSend
         mesh % nAdvCellsForEdge % recvList => mesh % nAdvCellsForEdge % block % parinfo % edgesToRecv
         mesh % nAdvCellsForEdge % copyList => mesh % nAdvCellsForEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % nAdvCellsForEdge % prev => prev % nAdvCellsForEdge
         else
           nullify(mesh % nAdvCellsForEdge % prev)
         end if
         if(present(next)) then
           mesh % nAdvCellsForEdge % next => next % nAdvCellsForEdge
         else
           nullify(mesh % nAdvCellsForEdge % next)
         end if


         mesh % highOrderAdvectionMask % sendList => mesh % highOrderAdvectionMask % block % parinfo % edgesToSend
         mesh % highOrderAdvectionMask % recvList => mesh % highOrderAdvectionMask % block % parinfo % edgesToRecv
         mesh % highOrderAdvectionMask % copyList => mesh % highOrderAdvectionMask % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % highOrderAdvectionMask % prev => prev % highOrderAdvectionMask
         else
           nullify(mesh % highOrderAdvectionMask % prev)
         end if
         if(present(next)) then
           mesh % highOrderAdvectionMask % next => next % highOrderAdvectionMask
         else
           nullify(mesh % highOrderAdvectionMask % next)
         end if


         mesh % coeffs_reconstruct % sendList => mesh % coeffs_reconstruct % block % parinfo % cellsToSend
         mesh % coeffs_reconstruct % recvList => mesh % coeffs_reconstruct % block % parinfo % cellsToRecv
         mesh % coeffs_reconstruct % copyList => mesh % coeffs_reconstruct % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % coeffs_reconstruct % prev => prev % coeffs_reconstruct
         else
           nullify(mesh % coeffs_reconstruct % prev)
         end if
         if(present(next)) then
           mesh % coeffs_reconstruct % next => next % coeffs_reconstruct
         else
           nullify(mesh % coeffs_reconstruct % next)
         end if


         mesh % maxLevelCell % sendList => mesh % maxLevelCell % block % parinfo % cellsToSend
         mesh % maxLevelCell % recvList => mesh % maxLevelCell % block % parinfo % cellsToRecv
         mesh % maxLevelCell % copyList => mesh % maxLevelCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % maxLevelCell % prev => prev % maxLevelCell
         else
           nullify(mesh % maxLevelCell % prev)
         end if
         if(present(next)) then
           mesh % maxLevelCell % next => next % maxLevelCell
         else
           nullify(mesh % maxLevelCell % next)
         end if


         mesh % maxLevelEdgeTop % sendList => mesh % maxLevelEdgeTop % block % parinfo % edgesToSend
         mesh % maxLevelEdgeTop % recvList => mesh % maxLevelEdgeTop % block % parinfo % edgesToRecv
         mesh % maxLevelEdgeTop % copyList => mesh % maxLevelEdgeTop % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % maxLevelEdgeTop % prev => prev % maxLevelEdgeTop
         else
           nullify(mesh % maxLevelEdgeTop % prev)
         end if
         if(present(next)) then
           mesh % maxLevelEdgeTop % next => next % maxLevelEdgeTop
         else
           nullify(mesh % maxLevelEdgeTop % next)
         end if


         mesh % maxLevelEdgeBot % sendList => mesh % maxLevelEdgeBot % block % parinfo % edgesToSend
         mesh % maxLevelEdgeBot % recvList => mesh % maxLevelEdgeBot % block % parinfo % edgesToRecv
         mesh % maxLevelEdgeBot % copyList => mesh % maxLevelEdgeBot % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % maxLevelEdgeBot % prev => prev % maxLevelEdgeBot
         else
           nullify(mesh % maxLevelEdgeBot % prev)
         end if
         if(present(next)) then
           mesh % maxLevelEdgeBot % next => next % maxLevelEdgeBot
         else
           nullify(mesh % maxLevelEdgeBot % next)
         end if


         mesh % maxLevelVertexTop % sendList => mesh % maxLevelVertexTop % block % parinfo % verticesToSend
         mesh % maxLevelVertexTop % recvList => mesh % maxLevelVertexTop % block % parinfo % verticesToRecv
         mesh % maxLevelVertexTop % copyList => mesh % maxLevelVertexTop % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % maxLevelVertexTop % prev => prev % maxLevelVertexTop
         else
           nullify(mesh % maxLevelVertexTop % prev)
         end if
         if(present(next)) then
           mesh % maxLevelVertexTop % next => next % maxLevelVertexTop
         else
           nullify(mesh % maxLevelVertexTop % next)
         end if


         mesh % maxLevelVertexBot % sendList => mesh % maxLevelVertexBot % block % parinfo % verticesToSend
         mesh % maxLevelVertexBot % recvList => mesh % maxLevelVertexBot % block % parinfo % verticesToRecv
         mesh % maxLevelVertexBot % copyList => mesh % maxLevelVertexBot % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % maxLevelVertexBot % prev => prev % maxLevelVertexBot
         else
           nullify(mesh % maxLevelVertexBot % prev)
         end if
         if(present(next)) then
           mesh % maxLevelVertexBot % next => next % maxLevelVertexBot
         else
           nullify(mesh % maxLevelVertexBot % next)
         end if


         nullify(mesh % refBottomDepth % sendList)
         nullify(mesh % refBottomDepth % recvList)
         nullify(mesh % refBottomDepth % copyList)
         if(present(prev)) then
           mesh % refBottomDepth % prev => prev % refBottomDepth
         else
           nullify(mesh % refBottomDepth % prev)
         end if
         if(present(next)) then
           mesh % refBottomDepth % next => next % refBottomDepth
         else
           nullify(mesh % refBottomDepth % next)
         end if


         nullify(mesh % refBottomDepthTopOfCell % sendList)
         nullify(mesh % refBottomDepthTopOfCell % recvList)
         nullify(mesh % refBottomDepthTopOfCell % copyList)
         if(present(prev)) then
           mesh % refBottomDepthTopOfCell % prev => prev % refBottomDepthTopOfCell
         else
           nullify(mesh % refBottomDepthTopOfCell % prev)
         end if
         if(present(next)) then
           mesh % refBottomDepthTopOfCell % next => next % refBottomDepthTopOfCell
         else
           nullify(mesh % refBottomDepthTopOfCell % next)
         end if


         nullify(mesh % vertCoordMovementWeights % sendList)
         nullify(mesh % vertCoordMovementWeights % recvList)
         nullify(mesh % vertCoordMovementWeights % copyList)
         if(present(prev)) then
           mesh % vertCoordMovementWeights % prev => prev % vertCoordMovementWeights
         else
           nullify(mesh % vertCoordMovementWeights % prev)
         end if
         if(present(next)) then
           mesh % vertCoordMovementWeights % next => next % vertCoordMovementWeights
         else
           nullify(mesh % vertCoordMovementWeights % next)
         end if


         mesh % boundaryEdge % sendList => mesh % boundaryEdge % block % parinfo % edgesToSend
         mesh % boundaryEdge % recvList => mesh % boundaryEdge % block % parinfo % edgesToRecv
         mesh % boundaryEdge % copyList => mesh % boundaryEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % boundaryEdge % prev => prev % boundaryEdge
         else
           nullify(mesh % boundaryEdge % prev)
         end if
         if(present(next)) then
           mesh % boundaryEdge % next => next % boundaryEdge
         else
           nullify(mesh % boundaryEdge % next)
         end if


         mesh % boundaryVertex % sendList => mesh % boundaryVertex % block % parinfo % verticesToSend
         mesh % boundaryVertex % recvList => mesh % boundaryVertex % block % parinfo % verticesToRecv
         mesh % boundaryVertex % copyList => mesh % boundaryVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % boundaryVertex % prev => prev % boundaryVertex
         else
           nullify(mesh % boundaryVertex % prev)
         end if
         if(present(next)) then
           mesh % boundaryVertex % next => next % boundaryVertex
         else
           nullify(mesh % boundaryVertex % next)
         end if


         mesh % boundaryCell % sendList => mesh % boundaryCell % block % parinfo % cellsToSend
         mesh % boundaryCell % recvList => mesh % boundaryCell % block % parinfo % cellsToRecv
         mesh % boundaryCell % copyList => mesh % boundaryCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % boundaryCell % prev => prev % boundaryCell
         else
           nullify(mesh % boundaryCell % prev)
         end if
         if(present(next)) then
           mesh % boundaryCell % next => next % boundaryCell
         else
           nullify(mesh % boundaryCell % next)
         end if


         mesh % edgeMask % sendList => mesh % edgeMask % block % parinfo % edgesToSend
         mesh % edgeMask % recvList => mesh % edgeMask % block % parinfo % edgesToRecv
         mesh % edgeMask % copyList => mesh % edgeMask % block % parinfo % edgesToCopy
         if(present(prev)) then
           mesh % edgeMask % prev => prev % edgeMask
         else
           nullify(mesh % edgeMask % prev)
         end if
         if(present(next)) then
           mesh % edgeMask % next => next % edgeMask
         else
           nullify(mesh % edgeMask % next)
         end if


         mesh % vertexMask % sendList => mesh % vertexMask % block % parinfo % verticesToSend
         mesh % vertexMask % recvList => mesh % vertexMask % block % parinfo % verticesToRecv
         mesh % vertexMask % copyList => mesh % vertexMask % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % vertexMask % prev => prev % vertexMask
         else
           nullify(mesh % vertexMask % prev)
         end if
         if(present(next)) then
           mesh % vertexMask % next => next % vertexMask
         else
           nullify(mesh % vertexMask % next)
         end if


         mesh % cellMask % sendList => mesh % cellMask % block % parinfo % cellsToSend
         mesh % cellMask % recvList => mesh % cellMask % block % parinfo % cellsToRecv
         mesh % cellMask % copyList => mesh % cellMask % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % cellMask % prev => prev % cellMask
         else
           nullify(mesh % cellMask % prev)
         end if
         if(present(next)) then
           mesh % cellMask % next => next % cellMask
         else
           nullify(mesh % cellMask % next)
         end if


         mesh % temperatureRestore % sendList => mesh % temperatureRestore % block % parinfo % cellsToSend
         mesh % temperatureRestore % recvList => mesh % temperatureRestore % block % parinfo % cellsToRecv
         mesh % temperatureRestore % copyList => mesh % temperatureRestore % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % temperatureRestore % prev => prev % temperatureRestore
         else
           nullify(mesh % temperatureRestore % prev)
         end if
         if(present(next)) then
           mesh % temperatureRestore % next => next % temperatureRestore
         else
           nullify(mesh % temperatureRestore % next)
         end if


         mesh % salinityRestore % sendList => mesh % salinityRestore % block % parinfo % cellsToSend
         mesh % salinityRestore % recvList => mesh % salinityRestore % block % parinfo % cellsToRecv
         mesh % salinityRestore % copyList => mesh % salinityRestore % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % salinityRestore % prev => prev % salinityRestore
         else
           nullify(mesh % salinityRestore % prev)
         end if
         if(present(next)) then
           mesh % salinityRestore % next => next % salinityRestore
         else
           nullify(mesh % salinityRestore % next)
         end if


         mesh % edgeSignOnCell % sendList => mesh % edgeSignOnCell % block % parinfo % cellsToSend
         mesh % edgeSignOnCell % recvList => mesh % edgeSignOnCell % block % parinfo % cellsToRecv
         mesh % edgeSignOnCell % copyList => mesh % edgeSignOnCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % edgeSignOnCell % prev => prev % edgeSignOnCell
         else
           nullify(mesh % edgeSignOnCell % prev)
         end if
         if(present(next)) then
           mesh % edgeSignOnCell % next => next % edgeSignOnCell
         else
           nullify(mesh % edgeSignOnCell % next)
         end if


         mesh % edgeSignOnVertex % sendList => mesh % edgeSignOnVertex % block % parinfo % verticesToSend
         mesh % edgeSignOnVertex % recvList => mesh % edgeSignOnVertex % block % parinfo % verticesToRecv
         mesh % edgeSignOnVertex % copyList => mesh % edgeSignOnVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           mesh % edgeSignOnVertex % prev => prev % edgeSignOnVertex
         else
           nullify(mesh % edgeSignOnVertex % prev)
         end if
         if(present(next)) then
           mesh % edgeSignOnVertex % next => next % edgeSignOnVertex
         else
           nullify(mesh % edgeSignOnVertex % next)
         end if


         mesh % kiteIndexOnCell % sendList => mesh % kiteIndexOnCell % block % parinfo % cellsToSend
         mesh % kiteIndexOnCell % recvList => mesh % kiteIndexOnCell % block % parinfo % cellsToRecv
         mesh % kiteIndexOnCell % copyList => mesh % kiteIndexOnCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           mesh % kiteIndexOnCell % prev => prev % kiteIndexOnCell
         else
           nullify(mesh % kiteIndexOnCell % prev)
         end if
         if(present(next)) then
           mesh % kiteIndexOnCell % next => next % kiteIndexOnCell
         else
           nullify(mesh % kiteIndexOnCell % next)
         end if


      end subroutine mpas_create_mesh_links


      subroutine mpas_create_verticalMesh_links(verticalMesh, prev, next)

         implicit none
         type (verticalMesh_type), pointer :: verticalMesh
         type (verticalMesh_type), pointer, optional :: prev, next
         verticalMesh % restingThickness % sendList => verticalMesh % restingThickness % block % parinfo % cellsToSend
         verticalMesh % restingThickness % recvList => verticalMesh % restingThickness % block % parinfo % cellsToRecv
         verticalMesh % restingThickness % copyList => verticalMesh % restingThickness % block % parinfo % cellsToCopy
         if(present(prev)) then
           verticalMesh % restingThickness % prev => prev % restingThickness
         else
           nullify(verticalMesh % restingThickness % prev)
         end if
         if(present(next)) then
           verticalMesh % restingThickness % next => next % restingThickness
         else
           nullify(verticalMesh % restingThickness % next)
         end if


         nullify(verticalMesh % refZMid % sendList)
         nullify(verticalMesh % refZMid % recvList)
         nullify(verticalMesh % refZMid % copyList)
         if(present(prev)) then
           verticalMesh % refZMid % prev => prev % refZMid
         else
           nullify(verticalMesh % refZMid % prev)
         end if
         if(present(next)) then
           verticalMesh % refZMid % next => next % refZMid
         else
           nullify(verticalMesh % refZMid % next)
         end if


         nullify(verticalMesh % refLayerThickness % sendList)
         nullify(verticalMesh % refLayerThickness % recvList)
         nullify(verticalMesh % refLayerThickness % copyList)
         if(present(prev)) then
           verticalMesh % refLayerThickness % prev => prev % refLayerThickness
         else
           nullify(verticalMesh % refLayerThickness % prev)
         end if
         if(present(next)) then
           verticalMesh % refLayerThickness % next => next % refLayerThickness
         else
           nullify(verticalMesh % refLayerThickness % next)
         end if


      end subroutine mpas_create_verticalMesh_links


      subroutine mpas_create_tend_links(tend, prev, next)

         implicit none
         type (tend_type), pointer :: tend
         type (tend_type), pointer, optional :: prev, next
         tend % tracers % sendList => tend % tracers % block % parinfo % cellsToSend
         tend % tracers % recvList => tend % tracers % block % parinfo % cellsToRecv
         tend % tracers % copyList => tend % tracers % block % parinfo % cellsToCopy
         if(present(prev)) then
           tend % tracers % prev => prev % tracers
         else
           nullify(tend % tracers % prev)
         end if
         if(present(next)) then
           tend % tracers % next => next % tracers
         else
           nullify(tend % tracers % next)
         end if


         tend % normalVelocity % sendList => tend % normalVelocity % block % parinfo % edgesToSend
         tend % normalVelocity % recvList => tend % normalVelocity % block % parinfo % edgesToRecv
         tend % normalVelocity % copyList => tend % normalVelocity % block % parinfo % edgesToCopy
         if(present(prev)) then
           tend % normalVelocity % prev => prev % normalVelocity
         else
           nullify(tend % normalVelocity % prev)
         end if
         if(present(next)) then
           tend % normalVelocity % next => next % normalVelocity
         else
           nullify(tend % normalVelocity % next)
         end if


         tend % layerThickness % sendList => tend % layerThickness % block % parinfo % cellsToSend
         tend % layerThickness % recvList => tend % layerThickness % block % parinfo % cellsToRecv
         tend % layerThickness % copyList => tend % layerThickness % block % parinfo % cellsToCopy
         if(present(prev)) then
           tend % layerThickness % prev => prev % layerThickness
         else
           nullify(tend % layerThickness % prev)
         end if
         if(present(next)) then
           tend % layerThickness % next => next % layerThickness
         else
           nullify(tend % layerThickness % next)
         end if


         tend % ssh % sendList => tend % ssh % block % parinfo % cellsToSend
         tend % ssh % recvList => tend % ssh % block % parinfo % cellsToRecv
         tend % ssh % copyList => tend % ssh % block % parinfo % cellsToCopy
         if(present(prev)) then
           tend % ssh % prev => prev % ssh
         else
           nullify(tend % ssh % prev)
         end if
         if(present(next)) then
           tend % ssh % next => next % ssh
         else
           nullify(tend % ssh % next)
         end if


         tend % highFreqThickness % sendList => tend % highFreqThickness % block % parinfo % cellsToSend
         tend % highFreqThickness % recvList => tend % highFreqThickness % block % parinfo % cellsToRecv
         tend % highFreqThickness % copyList => tend % highFreqThickness % block % parinfo % cellsToCopy
         if(present(prev)) then
           tend % highFreqThickness % prev => prev % highFreqThickness
         else
           nullify(tend % highFreqThickness % prev)
         end if
         if(present(next)) then
           tend % highFreqThickness % next => next % highFreqThickness
         else
           nullify(tend % highFreqThickness % next)
         end if


         tend % lowFreqDivergence % sendList => tend % lowFreqDivergence % block % parinfo % cellsToSend
         tend % lowFreqDivergence % recvList => tend % lowFreqDivergence % block % parinfo % cellsToRecv
         tend % lowFreqDivergence % copyList => tend % lowFreqDivergence % block % parinfo % cellsToCopy
         if(present(prev)) then
           tend % lowFreqDivergence % prev => prev % lowFreqDivergence
         else
           nullify(tend % lowFreqDivergence % prev)
         end if
         if(present(next)) then
           tend % lowFreqDivergence % next => next % lowFreqDivergence
         else
           nullify(tend % lowFreqDivergence % next)
         end if


      end subroutine mpas_create_tend_links


      subroutine mpas_create_diagnostics_links(diagnostics, prev, next)

         implicit none
         type (diagnostics_type), pointer :: diagnostics
         type (diagnostics_type), pointer, optional :: prev, next
         diagnostics % tracersSurfaceValue % sendList => diagnostics % tracersSurfaceValue % block % parinfo % cellsToSend
         diagnostics % tracersSurfaceValue % recvList => diagnostics % tracersSurfaceValue % block % parinfo % cellsToRecv
         diagnostics % tracersSurfaceValue % copyList => diagnostics % tracersSurfaceValue % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % tracersSurfaceValue % prev => prev % tracersSurfaceValue
         else
           nullify(diagnostics % tracersSurfaceValue % prev)
         end if
         if(present(next)) then
           diagnostics % tracersSurfaceValue % next => next % tracersSurfaceValue
         else
           nullify(diagnostics % tracersSurfaceValue % next)
         end if


         diagnostics % surfaceVelocity % sendList => diagnostics % surfaceVelocity % block % parinfo % cellsToSend
         diagnostics % surfaceVelocity % recvList => diagnostics % surfaceVelocity % block % parinfo % cellsToRecv
         diagnostics % surfaceVelocity % copyList => diagnostics % surfaceVelocity % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % surfaceVelocity % prev => prev % surfaceVelocity
         else
           nullify(diagnostics % surfaceVelocity % prev)
         end if
         if(present(next)) then
           diagnostics % surfaceVelocity % next => next % surfaceVelocity
         else
           nullify(diagnostics % surfaceVelocity % next)
         end if


         diagnostics % SSHGradient % sendList => diagnostics % SSHGradient % block % parinfo % cellsToSend
         diagnostics % SSHGradient % recvList => diagnostics % SSHGradient % block % parinfo % cellsToRecv
         diagnostics % SSHGradient % copyList => diagnostics % SSHGradient % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % SSHGradient % prev => prev % SSHGradient
         else
           nullify(diagnostics % SSHGradient % prev)
         end if
         if(present(next)) then
           diagnostics % SSHGradient % next => next % SSHGradient
         else
           nullify(diagnostics % SSHGradient % next)
         end if


         diagnostics % zMid % sendList => diagnostics % zMid % block % parinfo % cellsToSend
         diagnostics % zMid % recvList => diagnostics % zMid % block % parinfo % cellsToRecv
         diagnostics % zMid % copyList => diagnostics % zMid % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % zMid % prev => prev % zMid
         else
           nullify(diagnostics % zMid % prev)
         end if
         if(present(next)) then
           diagnostics % zMid % next => next % zMid
         else
           nullify(diagnostics % zMid % next)
         end if


         diagnostics % zTop % sendList => diagnostics % zTop % block % parinfo % cellsToSend
         diagnostics % zTop % recvList => diagnostics % zTop % block % parinfo % cellsToRecv
         diagnostics % zTop % copyList => diagnostics % zTop % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % zTop % prev => prev % zTop
         else
           nullify(diagnostics % zTop % prev)
         end if
         if(present(next)) then
           diagnostics % zTop % next => next % zTop
         else
           nullify(diagnostics % zTop % next)
         end if


         diagnostics % density % sendList => diagnostics % density % block % parinfo % cellsToSend
         diagnostics % density % recvList => diagnostics % density % block % parinfo % cellsToRecv
         diagnostics % density % copyList => diagnostics % density % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % density % prev => prev % density
         else
           nullify(diagnostics % density % prev)
         end if
         if(present(next)) then
           diagnostics % density % next => next % density
         else
           nullify(diagnostics % density % next)
         end if


         diagnostics % displacedDensity % sendList => diagnostics % displacedDensity % block % parinfo % cellsToSend
         diagnostics % displacedDensity % recvList => diagnostics % displacedDensity % block % parinfo % cellsToRecv
         diagnostics % displacedDensity % copyList => diagnostics % displacedDensity % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % displacedDensity % prev => prev % displacedDensity
         else
           nullify(diagnostics % displacedDensity % prev)
         end if
         if(present(next)) then
           diagnostics % displacedDensity % next => next % displacedDensity
         else
           nullify(diagnostics % displacedDensity % next)
         end if


         diagnostics % potentialDensity % sendList => diagnostics % potentialDensity % block % parinfo % cellsToSend
         diagnostics % potentialDensity % recvList => diagnostics % potentialDensity % block % parinfo % cellsToRecv
         diagnostics % potentialDensity % copyList => diagnostics % potentialDensity % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % potentialDensity % prev => prev % potentialDensity
         else
           nullify(diagnostics % potentialDensity % prev)
         end if
         if(present(next)) then
           diagnostics % potentialDensity % next => next % potentialDensity
         else
           nullify(diagnostics % potentialDensity % next)
         end if


         diagnostics % BruntVaisalaFreqTop % sendList => diagnostics % BruntVaisalaFreqTop % block % parinfo % cellsToSend
         diagnostics % BruntVaisalaFreqTop % recvList => diagnostics % BruntVaisalaFreqTop % block % parinfo % cellsToRecv
         diagnostics % BruntVaisalaFreqTop % copyList => diagnostics % BruntVaisalaFreqTop % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % BruntVaisalaFreqTop % prev => prev % BruntVaisalaFreqTop
         else
           nullify(diagnostics % BruntVaisalaFreqTop % prev)
         end if
         if(present(next)) then
           diagnostics % BruntVaisalaFreqTop % next => next % BruntVaisalaFreqTop
         else
           nullify(diagnostics % BruntVaisalaFreqTop % next)
         end if


         diagnostics % montgomeryPotential % sendList => diagnostics % montgomeryPotential % block % parinfo % cellsToSend
         diagnostics % montgomeryPotential % recvList => diagnostics % montgomeryPotential % block % parinfo % cellsToRecv
         diagnostics % montgomeryPotential % copyList => diagnostics % montgomeryPotential % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % montgomeryPotential % prev => prev % montgomeryPotential
         else
           nullify(diagnostics % montgomeryPotential % prev)
         end if
         if(present(next)) then
           diagnostics % montgomeryPotential % next => next % montgomeryPotential
         else
           nullify(diagnostics % montgomeryPotential % next)
         end if


         diagnostics % pressure % sendList => diagnostics % pressure % block % parinfo % cellsToSend
         diagnostics % pressure % recvList => diagnostics % pressure % block % parinfo % cellsToRecv
         diagnostics % pressure % copyList => diagnostics % pressure % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % pressure % prev => prev % pressure
         else
           nullify(diagnostics % pressure % prev)
         end if
         if(present(next)) then
           diagnostics % pressure % next => next % pressure
         else
           nullify(diagnostics % pressure % next)
         end if


         diagnostics % uTransport % sendList => diagnostics % uTransport % block % parinfo % edgesToSend
         diagnostics % uTransport % recvList => diagnostics % uTransport % block % parinfo % edgesToRecv
         diagnostics % uTransport % copyList => diagnostics % uTransport % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % uTransport % prev => prev % uTransport
         else
           nullify(diagnostics % uTransport % prev)
         end if
         if(present(next)) then
           diagnostics % uTransport % next => next % uTransport
         else
           nullify(diagnostics % uTransport % next)
         end if


         diagnostics % vertTransportVelocityTop % sendList => diagnostics % vertTransportVelocityTop % block % parinfo % &
cellsToSend
         diagnostics % vertTransportVelocityTop % recvList => diagnostics % vertTransportVelocityTop % block % parinfo % &
cellsToRecv
         diagnostics % vertTransportVelocityTop % copyList => diagnostics % vertTransportVelocityTop % block % parinfo % &
cellsToCopy
         if(present(prev)) then
           diagnostics % vertTransportVelocityTop % prev => prev % vertTransportVelocityTop
         else
           nullify(diagnostics % vertTransportVelocityTop % prev)
         end if
         if(present(next)) then
           diagnostics % vertTransportVelocityTop % next => next % vertTransportVelocityTop
         else
           nullify(diagnostics % vertTransportVelocityTop % next)
         end if


         diagnostics % vertVelocityTop % sendList => diagnostics % vertVelocityTop % block % parinfo % cellsToSend
         diagnostics % vertVelocityTop % recvList => diagnostics % vertVelocityTop % block % parinfo % cellsToRecv
         diagnostics % vertVelocityTop % copyList => diagnostics % vertVelocityTop % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % vertVelocityTop % prev => prev % vertVelocityTop
         else
           nullify(diagnostics % vertVelocityTop % prev)
         end if
         if(present(next)) then
           diagnostics % vertVelocityTop % next => next % vertVelocityTop
         else
           nullify(diagnostics % vertVelocityTop % next)
         end if


         diagnostics % tangentialVelocity % sendList => diagnostics % tangentialVelocity % block % parinfo % edgesToSend
         diagnostics % tangentialVelocity % recvList => diagnostics % tangentialVelocity % block % parinfo % edgesToRecv
         diagnostics % tangentialVelocity % copyList => diagnostics % tangentialVelocity % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % tangentialVelocity % prev => prev % tangentialVelocity
         else
           nullify(diagnostics % tangentialVelocity % prev)
         end if
         if(present(next)) then
           diagnostics % tangentialVelocity % next => next % tangentialVelocity
         else
           nullify(diagnostics % tangentialVelocity % next)
         end if


         diagnostics % layerThicknessEdge % sendList => diagnostics % layerThicknessEdge % block % parinfo % edgesToSend
         diagnostics % layerThicknessEdge % recvList => diagnostics % layerThicknessEdge % block % parinfo % edgesToRecv
         diagnostics % layerThicknessEdge % copyList => diagnostics % layerThicknessEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % layerThicknessEdge % prev => prev % layerThicknessEdge
         else
           nullify(diagnostics % layerThicknessEdge % prev)
         end if
         if(present(next)) then
           diagnostics % layerThicknessEdge % next => next % layerThicknessEdge
         else
           nullify(diagnostics % layerThicknessEdge % next)
         end if


         diagnostics % layerThicknessVertex % sendList => diagnostics % layerThicknessVertex % block % parinfo % verticesToSend
         diagnostics % layerThicknessVertex % recvList => diagnostics % layerThicknessVertex % block % parinfo % verticesToRecv
         diagnostics % layerThicknessVertex % copyList => diagnostics % layerThicknessVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           diagnostics % layerThicknessVertex % prev => prev % layerThicknessVertex
         else
           nullify(diagnostics % layerThicknessVertex % prev)
         end if
         if(present(next)) then
           diagnostics % layerThicknessVertex % next => next % layerThicknessVertex
         else
           nullify(diagnostics % layerThicknessVertex % next)
         end if


         diagnostics % kineticEnergyCell % sendList => diagnostics % kineticEnergyCell % block % parinfo % cellsToSend
         diagnostics % kineticEnergyCell % recvList => diagnostics % kineticEnergyCell % block % parinfo % cellsToRecv
         diagnostics % kineticEnergyCell % copyList => diagnostics % kineticEnergyCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % kineticEnergyCell % prev => prev % kineticEnergyCell
         else
           nullify(diagnostics % kineticEnergyCell % prev)
         end if
         if(present(next)) then
           diagnostics % kineticEnergyCell % next => next % kineticEnergyCell
         else
           nullify(diagnostics % kineticEnergyCell % next)
         end if


         diagnostics % hEddyFlux % sendList => diagnostics % hEddyFlux % block % parinfo % edgesToSend
         diagnostics % hEddyFlux % recvList => diagnostics % hEddyFlux % block % parinfo % edgesToRecv
         diagnostics % hEddyFlux % copyList => diagnostics % hEddyFlux % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % hEddyFlux % prev => prev % hEddyFlux
         else
           nullify(diagnostics % hEddyFlux % prev)
         end if
         if(present(next)) then
           diagnostics % hEddyFlux % next => next % hEddyFlux
         else
           nullify(diagnostics % hEddyFlux % next)
         end if


         diagnostics % hKappa % sendList => diagnostics % hKappa % block % parinfo % edgesToSend
         diagnostics % hKappa % recvList => diagnostics % hKappa % block % parinfo % edgesToRecv
         diagnostics % hKappa % copyList => diagnostics % hKappa % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % hKappa % prev => prev % hKappa
         else
           nullify(diagnostics % hKappa % prev)
         end if
         if(present(next)) then
           diagnostics % hKappa % next => next % hKappa
         else
           nullify(diagnostics % hKappa % next)
         end if


         diagnostics % hKappaQ % sendList => diagnostics % hKappaQ % block % parinfo % edgesToSend
         diagnostics % hKappaQ % recvList => diagnostics % hKappaQ % block % parinfo % edgesToRecv
         diagnostics % hKappaQ % copyList => diagnostics % hKappaQ % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % hKappaQ % prev => prev % hKappaQ
         else
           nullify(diagnostics % hKappaQ % prev)
         end if
         if(present(next)) then
           diagnostics % hKappaQ % next => next % hKappaQ
         else
           nullify(diagnostics % hKappaQ % next)
         end if


         diagnostics % viscosity % sendList => diagnostics % viscosity % block % parinfo % edgesToSend
         diagnostics % viscosity % recvList => diagnostics % viscosity % block % parinfo % edgesToRecv
         diagnostics % viscosity % copyList => diagnostics % viscosity % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % viscosity % prev => prev % viscosity
         else
           nullify(diagnostics % viscosity % prev)
         end if
         if(present(next)) then
           diagnostics % viscosity % next => next % viscosity
         else
           nullify(diagnostics % viscosity % next)
         end if


         diagnostics % divergence % sendList => diagnostics % divergence % block % parinfo % cellsToSend
         diagnostics % divergence % recvList => diagnostics % divergence % block % parinfo % cellsToRecv
         diagnostics % divergence % copyList => diagnostics % divergence % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % divergence % prev => prev % divergence
         else
           nullify(diagnostics % divergence % prev)
         end if
         if(present(next)) then
           diagnostics % divergence % next => next % divergence
         else
           nullify(diagnostics % divergence % next)
         end if


         diagnostics % circulation % sendList => diagnostics % circulation % block % parinfo % verticesToSend
         diagnostics % circulation % recvList => diagnostics % circulation % block % parinfo % verticesToRecv
         diagnostics % circulation % copyList => diagnostics % circulation % block % parinfo % verticesToCopy
         if(present(prev)) then
           diagnostics % circulation % prev => prev % circulation
         else
           nullify(diagnostics % circulation % prev)
         end if
         if(present(next)) then
           diagnostics % circulation % next => next % circulation
         else
           nullify(diagnostics % circulation % next)
         end if


         diagnostics % relativeVorticity % sendList => diagnostics % relativeVorticity % block % parinfo % verticesToSend
         diagnostics % relativeVorticity % recvList => diagnostics % relativeVorticity % block % parinfo % verticesToRecv
         diagnostics % relativeVorticity % copyList => diagnostics % relativeVorticity % block % parinfo % verticesToCopy
         if(present(prev)) then
           diagnostics % relativeVorticity % prev => prev % relativeVorticity
         else
           nullify(diagnostics % relativeVorticity % prev)
         end if
         if(present(next)) then
           diagnostics % relativeVorticity % next => next % relativeVorticity
         else
           nullify(diagnostics % relativeVorticity % next)
         end if


         diagnostics % relativeVorticityCell % sendList => diagnostics % relativeVorticityCell % block % parinfo % cellsToSend
         diagnostics % relativeVorticityCell % recvList => diagnostics % relativeVorticityCell % block % parinfo % cellsToRecv
         diagnostics % relativeVorticityCell % copyList => diagnostics % relativeVorticityCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % relativeVorticityCell % prev => prev % relativeVorticityCell
         else
           nullify(diagnostics % relativeVorticityCell % prev)
         end if
         if(present(next)) then
           diagnostics % relativeVorticityCell % next => next % relativeVorticityCell
         else
           nullify(diagnostics % relativeVorticityCell % next)
         end if


         diagnostics % normalizedRelativeVorticityEdge % sendList => diagnostics % normalizedRelativeVorticityEdge % block % &
parinfo % edgesToSend
         diagnostics % normalizedRelativeVorticityEdge % recvList => diagnostics % normalizedRelativeVorticityEdge % block % &
parinfo % edgesToRecv
         diagnostics % normalizedRelativeVorticityEdge % copyList => diagnostics % normalizedRelativeVorticityEdge % block % &
parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % normalizedRelativeVorticityEdge % prev => prev % normalizedRelativeVorticityEdge
         else
           nullify(diagnostics % normalizedRelativeVorticityEdge % prev)
         end if
         if(present(next)) then
           diagnostics % normalizedRelativeVorticityEdge % next => next % normalizedRelativeVorticityEdge
         else
           nullify(diagnostics % normalizedRelativeVorticityEdge % next)
         end if


         diagnostics % normalizedPlanetaryVorticityEdge % sendList => diagnostics % normalizedPlanetaryVorticityEdge % block % &
parinfo % edgesToSend
         diagnostics % normalizedPlanetaryVorticityEdge % recvList => diagnostics % normalizedPlanetaryVorticityEdge % block % &
parinfo % edgesToRecv
         diagnostics % normalizedPlanetaryVorticityEdge % copyList => diagnostics % normalizedPlanetaryVorticityEdge % block % &
parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % normalizedPlanetaryVorticityEdge % prev => prev % normalizedPlanetaryVorticityEdge
         else
           nullify(diagnostics % normalizedPlanetaryVorticityEdge % prev)
         end if
         if(present(next)) then
           diagnostics % normalizedPlanetaryVorticityEdge % next => next % normalizedPlanetaryVorticityEdge
         else
           nullify(diagnostics % normalizedPlanetaryVorticityEdge % next)
         end if


         diagnostics % normalizedRelativeVorticityCell % sendList => diagnostics % normalizedRelativeVorticityCell % block % &
parinfo % cellsToSend
         diagnostics % normalizedRelativeVorticityCell % recvList => diagnostics % normalizedRelativeVorticityCell % block % &
parinfo % cellsToRecv
         diagnostics % normalizedRelativeVorticityCell % copyList => diagnostics % normalizedRelativeVorticityCell % block % &
parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % normalizedRelativeVorticityCell % prev => prev % normalizedRelativeVorticityCell
         else
           nullify(diagnostics % normalizedRelativeVorticityCell % prev)
         end if
         if(present(next)) then
           diagnostics % normalizedRelativeVorticityCell % next => next % normalizedRelativeVorticityCell
         else
           nullify(diagnostics % normalizedRelativeVorticityCell % next)
         end if


         diagnostics % barotropicForcing % sendList => diagnostics % barotropicForcing % block % parinfo % edgesToSend
         diagnostics % barotropicForcing % recvList => diagnostics % barotropicForcing % block % parinfo % edgesToRecv
         diagnostics % barotropicForcing % copyList => diagnostics % barotropicForcing % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % barotropicForcing % prev => prev % barotropicForcing
         else
           nullify(diagnostics % barotropicForcing % prev)
         end if
         if(present(next)) then
           diagnostics % barotropicForcing % next => next % barotropicForcing
         else
           nullify(diagnostics % barotropicForcing % next)
         end if


         diagnostics % barotropicThicknessFlux % sendList => diagnostics % barotropicThicknessFlux % block % parinfo % edgesToSend
         diagnostics % barotropicThicknessFlux % recvList => diagnostics % barotropicThicknessFlux % block % parinfo % edgesToRecv
         diagnostics % barotropicThicknessFlux % copyList => diagnostics % barotropicThicknessFlux % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % barotropicThicknessFlux % prev => prev % barotropicThicknessFlux
         else
           nullify(diagnostics % barotropicThicknessFlux % prev)
         end if
         if(present(next)) then
           diagnostics % barotropicThicknessFlux % next => next % barotropicThicknessFlux
         else
           nullify(diagnostics % barotropicThicknessFlux % next)
         end if


         diagnostics % normalVelocityX % sendList => diagnostics % normalVelocityX % block % parinfo % cellsToSend
         diagnostics % normalVelocityX % recvList => diagnostics % normalVelocityX % block % parinfo % cellsToRecv
         diagnostics % normalVelocityX % copyList => diagnostics % normalVelocityX % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % normalVelocityX % prev => prev % normalVelocityX
         else
           nullify(diagnostics % normalVelocityX % prev)
         end if
         if(present(next)) then
           diagnostics % normalVelocityX % next => next % normalVelocityX
         else
           nullify(diagnostics % normalVelocityX % next)
         end if


         diagnostics % normalVelocityY % sendList => diagnostics % normalVelocityY % block % parinfo % cellsToSend
         diagnostics % normalVelocityY % recvList => diagnostics % normalVelocityY % block % parinfo % cellsToRecv
         diagnostics % normalVelocityY % copyList => diagnostics % normalVelocityY % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % normalVelocityY % prev => prev % normalVelocityY
         else
           nullify(diagnostics % normalVelocityY % prev)
         end if
         if(present(next)) then
           diagnostics % normalVelocityY % next => next % normalVelocityY
         else
           nullify(diagnostics % normalVelocityY % next)
         end if


         diagnostics % normalVelocityZ % sendList => diagnostics % normalVelocityZ % block % parinfo % cellsToSend
         diagnostics % normalVelocityZ % recvList => diagnostics % normalVelocityZ % block % parinfo % cellsToRecv
         diagnostics % normalVelocityZ % copyList => diagnostics % normalVelocityZ % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % normalVelocityZ % prev => prev % normalVelocityZ
         else
           nullify(diagnostics % normalVelocityZ % prev)
         end if
         if(present(next)) then
           diagnostics % normalVelocityZ % next => next % normalVelocityZ
         else
           nullify(diagnostics % normalVelocityZ % next)
         end if


         diagnostics % normalVelocityZonal % sendList => diagnostics % normalVelocityZonal % block % parinfo % cellsToSend
         diagnostics % normalVelocityZonal % recvList => diagnostics % normalVelocityZonal % block % parinfo % cellsToRecv
         diagnostics % normalVelocityZonal % copyList => diagnostics % normalVelocityZonal % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % normalVelocityZonal % prev => prev % normalVelocityZonal
         else
           nullify(diagnostics % normalVelocityZonal % prev)
         end if
         if(present(next)) then
           diagnostics % normalVelocityZonal % next => next % normalVelocityZonal
         else
           nullify(diagnostics % normalVelocityZonal % next)
         end if


         diagnostics % normalVelocityMeridional % sendList => diagnostics % normalVelocityMeridional % block % parinfo % &
cellsToSend
         diagnostics % normalVelocityMeridional % recvList => diagnostics % normalVelocityMeridional % block % parinfo % &
cellsToRecv
         diagnostics % normalVelocityMeridional % copyList => diagnostics % normalVelocityMeridional % block % parinfo % &
cellsToCopy
         if(present(prev)) then
           diagnostics % normalVelocityMeridional % prev => prev % normalVelocityMeridional
         else
           nullify(diagnostics % normalVelocityMeridional % prev)
         end if
         if(present(next)) then
           diagnostics % normalVelocityMeridional % next => next % normalVelocityMeridional
         else
           nullify(diagnostics % normalVelocityMeridional % next)
         end if


         diagnostics % gradSSH % sendList => diagnostics % gradSSH % block % parinfo % edgesToSend
         diagnostics % gradSSH % recvList => diagnostics % gradSSH % block % parinfo % edgesToRecv
         diagnostics % gradSSH % copyList => diagnostics % gradSSH % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % gradSSH % prev => prev % gradSSH
         else
           nullify(diagnostics % gradSSH % prev)
         end if
         if(present(next)) then
           diagnostics % gradSSH % next => next % gradSSH
         else
           nullify(diagnostics % gradSSH % next)
         end if


         diagnostics % gradSSHX % sendList => diagnostics % gradSSHX % block % parinfo % cellsToSend
         diagnostics % gradSSHX % recvList => diagnostics % gradSSHX % block % parinfo % cellsToRecv
         diagnostics % gradSSHX % copyList => diagnostics % gradSSHX % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % gradSSHX % prev => prev % gradSSHX
         else
           nullify(diagnostics % gradSSHX % prev)
         end if
         if(present(next)) then
           diagnostics % gradSSHX % next => next % gradSSHX
         else
           nullify(diagnostics % gradSSHX % next)
         end if


         diagnostics % gradSSHY % sendList => diagnostics % gradSSHY % block % parinfo % cellsToSend
         diagnostics % gradSSHY % recvList => diagnostics % gradSSHY % block % parinfo % cellsToRecv
         diagnostics % gradSSHY % copyList => diagnostics % gradSSHY % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % gradSSHY % prev => prev % gradSSHY
         else
           nullify(diagnostics % gradSSHY % prev)
         end if
         if(present(next)) then
           diagnostics % gradSSHY % next => next % gradSSHY
         else
           nullify(diagnostics % gradSSHY % next)
         end if


         diagnostics % gradSSHZ % sendList => diagnostics % gradSSHZ % block % parinfo % cellsToSend
         diagnostics % gradSSHZ % recvList => diagnostics % gradSSHZ % block % parinfo % cellsToRecv
         diagnostics % gradSSHZ % copyList => diagnostics % gradSSHZ % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % gradSSHZ % prev => prev % gradSSHZ
         else
           nullify(diagnostics % gradSSHZ % prev)
         end if
         if(present(next)) then
           diagnostics % gradSSHZ % next => next % gradSSHZ
         else
           nullify(diagnostics % gradSSHZ % next)
         end if


         diagnostics % gradSSHZonal % sendList => diagnostics % gradSSHZonal % block % parinfo % cellsToSend
         diagnostics % gradSSHZonal % recvList => diagnostics % gradSSHZonal % block % parinfo % cellsToRecv
         diagnostics % gradSSHZonal % copyList => diagnostics % gradSSHZonal % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % gradSSHZonal % prev => prev % gradSSHZonal
         else
           nullify(diagnostics % gradSSHZonal % prev)
         end if
         if(present(next)) then
           diagnostics % gradSSHZonal % next => next % gradSSHZonal
         else
           nullify(diagnostics % gradSSHZonal % next)
         end if


         diagnostics % gradSSHMeridional % sendList => diagnostics % gradSSHMeridional % block % parinfo % cellsToSend
         diagnostics % gradSSHMeridional % recvList => diagnostics % gradSSHMeridional % block % parinfo % cellsToRecv
         diagnostics % gradSSHMeridional % copyList => diagnostics % gradSSHMeridional % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % gradSSHMeridional % prev => prev % gradSSHMeridional
         else
           nullify(diagnostics % gradSSHMeridional % prev)
         end if
         if(present(next)) then
           diagnostics % gradSSHMeridional % next => next % gradSSHMeridional
         else
           nullify(diagnostics % gradSSHMeridional % next)
         end if


         diagnostics % uBolusGM % sendList => diagnostics % uBolusGM % block % parinfo % edgesToSend
         diagnostics % uBolusGM % recvList => diagnostics % uBolusGM % block % parinfo % edgesToRecv
         diagnostics % uBolusGM % copyList => diagnostics % uBolusGM % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % uBolusGM % prev => prev % uBolusGM
         else
           nullify(diagnostics % uBolusGM % prev)
         end if
         if(present(next)) then
           diagnostics % uBolusGM % next => next % uBolusGM
         else
           nullify(diagnostics % uBolusGM % next)
         end if


         diagnostics % uBolusGMX % sendList => diagnostics % uBolusGMX % block % parinfo % edgesToSend
         diagnostics % uBolusGMX % recvList => diagnostics % uBolusGMX % block % parinfo % edgesToRecv
         diagnostics % uBolusGMX % copyList => diagnostics % uBolusGMX % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % uBolusGMX % prev => prev % uBolusGMX
         else
           nullify(diagnostics % uBolusGMX % prev)
         end if
         if(present(next)) then
           diagnostics % uBolusGMX % next => next % uBolusGMX
         else
           nullify(diagnostics % uBolusGMX % next)
         end if


         diagnostics % uBolusGMY % sendList => diagnostics % uBolusGMY % block % parinfo % edgesToSend
         diagnostics % uBolusGMY % recvList => diagnostics % uBolusGMY % block % parinfo % edgesToRecv
         diagnostics % uBolusGMY % copyList => diagnostics % uBolusGMY % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % uBolusGMY % prev => prev % uBolusGMY
         else
           nullify(diagnostics % uBolusGMY % prev)
         end if
         if(present(next)) then
           diagnostics % uBolusGMY % next => next % uBolusGMY
         else
           nullify(diagnostics % uBolusGMY % next)
         end if


         diagnostics % uBolusGMZ % sendList => diagnostics % uBolusGMZ % block % parinfo % edgesToSend
         diagnostics % uBolusGMZ % recvList => diagnostics % uBolusGMZ % block % parinfo % edgesToRecv
         diagnostics % uBolusGMZ % copyList => diagnostics % uBolusGMZ % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % uBolusGMZ % prev => prev % uBolusGMZ
         else
           nullify(diagnostics % uBolusGMZ % prev)
         end if
         if(present(next)) then
           diagnostics % uBolusGMZ % next => next % uBolusGMZ
         else
           nullify(diagnostics % uBolusGMZ % next)
         end if


         diagnostics % uBolusGMZonal % sendList => diagnostics % uBolusGMZonal % block % parinfo % edgesToSend
         diagnostics % uBolusGMZonal % recvList => diagnostics % uBolusGMZonal % block % parinfo % edgesToRecv
         diagnostics % uBolusGMZonal % copyList => diagnostics % uBolusGMZonal % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % uBolusGMZonal % prev => prev % uBolusGMZonal
         else
           nullify(diagnostics % uBolusGMZonal % prev)
         end if
         if(present(next)) then
           diagnostics % uBolusGMZonal % next => next % uBolusGMZonal
         else
           nullify(diagnostics % uBolusGMZonal % next)
         end if


         diagnostics % uBolusGMMeridional % sendList => diagnostics % uBolusGMMeridional % block % parinfo % edgesToSend
         diagnostics % uBolusGMMeridional % recvList => diagnostics % uBolusGMMeridional % block % parinfo % edgesToRecv
         diagnostics % uBolusGMMeridional % copyList => diagnostics % uBolusGMMeridional % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % uBolusGMMeridional % prev => prev % uBolusGMMeridional
         else
           nullify(diagnostics % uBolusGMMeridional % prev)
         end if
         if(present(next)) then
           diagnostics % uBolusGMMeridional % next => next % uBolusGMMeridional
         else
           nullify(diagnostics % uBolusGMMeridional % next)
         end if


         diagnostics % RiTopOfCell % sendList => diagnostics % RiTopOfCell % block % parinfo % cellsToSend
         diagnostics % RiTopOfCell % recvList => diagnostics % RiTopOfCell % block % parinfo % cellsToRecv
         diagnostics % RiTopOfCell % copyList => diagnostics % RiTopOfCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % RiTopOfCell % prev => prev % RiTopOfCell
         else
           nullify(diagnostics % RiTopOfCell % prev)
         end if
         if(present(next)) then
           diagnostics % RiTopOfCell % next => next % RiTopOfCell
         else
           nullify(diagnostics % RiTopOfCell % next)
         end if


         diagnostics % RiTopOfEdge % sendList => diagnostics % RiTopOfEdge % block % parinfo % edgesToSend
         diagnostics % RiTopOfEdge % recvList => diagnostics % RiTopOfEdge % block % parinfo % edgesToRecv
         diagnostics % RiTopOfEdge % copyList => diagnostics % RiTopOfEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % RiTopOfEdge % prev => prev % RiTopOfEdge
         else
           nullify(diagnostics % RiTopOfEdge % prev)
         end if
         if(present(next)) then
           diagnostics % RiTopOfEdge % next => next % RiTopOfEdge
         else
           nullify(diagnostics % RiTopOfEdge % next)
         end if


         diagnostics % vertViscTopOfEdge % sendList => diagnostics % vertViscTopOfEdge % block % parinfo % edgesToSend
         diagnostics % vertViscTopOfEdge % recvList => diagnostics % vertViscTopOfEdge % block % parinfo % edgesToRecv
         diagnostics % vertViscTopOfEdge % copyList => diagnostics % vertViscTopOfEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           diagnostics % vertViscTopOfEdge % prev => prev % vertViscTopOfEdge
         else
           nullify(diagnostics % vertViscTopOfEdge % prev)
         end if
         if(present(next)) then
           diagnostics % vertViscTopOfEdge % next => next % vertViscTopOfEdge
         else
           nullify(diagnostics % vertViscTopOfEdge % next)
         end if


         diagnostics % vertViscTopOfCell % sendList => diagnostics % vertViscTopOfCell % block % parinfo % cellsToSend
         diagnostics % vertViscTopOfCell % recvList => diagnostics % vertViscTopOfCell % block % parinfo % cellsToRecv
         diagnostics % vertViscTopOfCell % copyList => diagnostics % vertViscTopOfCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % vertViscTopOfCell % prev => prev % vertViscTopOfCell
         else
           nullify(diagnostics % vertViscTopOfCell % prev)
         end if
         if(present(next)) then
           diagnostics % vertViscTopOfCell % next => next % vertViscTopOfCell
         else
           nullify(diagnostics % vertViscTopOfCell % next)
         end if


         diagnostics % vertDiffTopOfCell % sendList => diagnostics % vertDiffTopOfCell % block % parinfo % cellsToSend
         diagnostics % vertDiffTopOfCell % recvList => diagnostics % vertDiffTopOfCell % block % parinfo % cellsToRecv
         diagnostics % vertDiffTopOfCell % copyList => diagnostics % vertDiffTopOfCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % vertDiffTopOfCell % prev => prev % vertDiffTopOfCell
         else
           nullify(diagnostics % vertDiffTopOfCell % prev)
         end if
         if(present(next)) then
           diagnostics % vertDiffTopOfCell % next => next % vertDiffTopOfCell
         else
           nullify(diagnostics % vertDiffTopOfCell % next)
         end if


         diagnostics % bulkRichardsonNumber % sendList => diagnostics % bulkRichardsonNumber % block % parinfo % cellsToSend
         diagnostics % bulkRichardsonNumber % recvList => diagnostics % bulkRichardsonNumber % block % parinfo % cellsToRecv
         diagnostics % bulkRichardsonNumber % copyList => diagnostics % bulkRichardsonNumber % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % bulkRichardsonNumber % prev => prev % bulkRichardsonNumber
         else
           nullify(diagnostics % bulkRichardsonNumber % prev)
         end if
         if(present(next)) then
           diagnostics % bulkRichardsonNumber % next => next % bulkRichardsonNumber
         else
           nullify(diagnostics % bulkRichardsonNumber % next)
         end if


         diagnostics % boundaryLayerDepth % sendList => diagnostics % boundaryLayerDepth % block % parinfo % cellsToSend
         diagnostics % boundaryLayerDepth % recvList => diagnostics % boundaryLayerDepth % block % parinfo % cellsToRecv
         diagnostics % boundaryLayerDepth % copyList => diagnostics % boundaryLayerDepth % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % boundaryLayerDepth % prev => prev % boundaryLayerDepth
         else
           nullify(diagnostics % boundaryLayerDepth % prev)
         end if
         if(present(next)) then
           diagnostics % boundaryLayerDepth % next => next % boundaryLayerDepth
         else
           nullify(diagnostics % boundaryLayerDepth % next)
         end if


         diagnostics % indexBoundaryLayerDepth % sendList => diagnostics % indexBoundaryLayerDepth % block % parinfo % cellsToSend
         diagnostics % indexBoundaryLayerDepth % recvList => diagnostics % indexBoundaryLayerDepth % block % parinfo % cellsToRecv
         diagnostics % indexBoundaryLayerDepth % copyList => diagnostics % indexBoundaryLayerDepth % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % indexBoundaryLayerDepth % prev => prev % indexBoundaryLayerDepth
         else
           nullify(diagnostics % indexBoundaryLayerDepth % prev)
         end if
         if(present(next)) then
           diagnostics % indexBoundaryLayerDepth % next => next % indexBoundaryLayerDepth
         else
           nullify(diagnostics % indexBoundaryLayerDepth % next)
         end if


         diagnostics % surfaceFrictionVelocity % sendList => diagnostics % surfaceFrictionVelocity % block % parinfo % cellsToSend
         diagnostics % surfaceFrictionVelocity % recvList => diagnostics % surfaceFrictionVelocity % block % parinfo % cellsToRecv
         diagnostics % surfaceFrictionVelocity % copyList => diagnostics % surfaceFrictionVelocity % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % surfaceFrictionVelocity % prev => prev % surfaceFrictionVelocity
         else
           nullify(diagnostics % surfaceFrictionVelocity % prev)
         end if
         if(present(next)) then
           diagnostics % surfaceFrictionVelocity % next => next % surfaceFrictionVelocity
         else
           nullify(diagnostics % surfaceFrictionVelocity % next)
         end if


         diagnostics % windStressZonalDiag % sendList => diagnostics % windStressZonalDiag % block % parinfo % cellsToSend
         diagnostics % windStressZonalDiag % recvList => diagnostics % windStressZonalDiag % block % parinfo % cellsToRecv
         diagnostics % windStressZonalDiag % copyList => diagnostics % windStressZonalDiag % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % windStressZonalDiag % prev => prev % windStressZonalDiag
         else
           nullify(diagnostics % windStressZonalDiag % prev)
         end if
         if(present(next)) then
           diagnostics % windStressZonalDiag % next => next % windStressZonalDiag
         else
           nullify(diagnostics % windStressZonalDiag % next)
         end if


         diagnostics % windStressMeridionalDiag % sendList => diagnostics % windStressMeridionalDiag % block % parinfo % &
cellsToSend
         diagnostics % windStressMeridionalDiag % recvList => diagnostics % windStressMeridionalDiag % block % parinfo % &
cellsToRecv
         diagnostics % windStressMeridionalDiag % copyList => diagnostics % windStressMeridionalDiag % block % parinfo % &
cellsToCopy
         if(present(prev)) then
           diagnostics % windStressMeridionalDiag % prev => prev % windStressMeridionalDiag
         else
           nullify(diagnostics % windStressMeridionalDiag % prev)
         end if
         if(present(next)) then
           diagnostics % windStressMeridionalDiag % next => next % windStressMeridionalDiag
         else
           nullify(diagnostics % windStressMeridionalDiag % next)
         end if


         diagnostics % penetrativeTemperatureFluxOBL % sendList => diagnostics % penetrativeTemperatureFluxOBL % block % parinfo % &
cellsToSend
         diagnostics % penetrativeTemperatureFluxOBL % recvList => diagnostics % penetrativeTemperatureFluxOBL % block % parinfo % &
cellsToRecv
         diagnostics % penetrativeTemperatureFluxOBL % copyList => diagnostics % penetrativeTemperatureFluxOBL % block % parinfo % &
cellsToCopy
         if(present(prev)) then
           diagnostics % penetrativeTemperatureFluxOBL % prev => prev % penetrativeTemperatureFluxOBL
         else
           nullify(diagnostics % penetrativeTemperatureFluxOBL % prev)
         end if
         if(present(next)) then
           diagnostics % penetrativeTemperatureFluxOBL % next => next % penetrativeTemperatureFluxOBL
         else
           nullify(diagnostics % penetrativeTemperatureFluxOBL % next)
         end if


         diagnostics % buoyancyForcingOBL % sendList => diagnostics % buoyancyForcingOBL % block % parinfo % cellsToSend
         diagnostics % buoyancyForcingOBL % recvList => diagnostics % buoyancyForcingOBL % block % parinfo % cellsToRecv
         diagnostics % buoyancyForcingOBL % copyList => diagnostics % buoyancyForcingOBL % block % parinfo % cellsToCopy
         if(present(prev)) then
           diagnostics % buoyancyForcingOBL % prev => prev % buoyancyForcingOBL
         else
           nullify(diagnostics % buoyancyForcingOBL % prev)
         end if
         if(present(next)) then
           diagnostics % buoyancyForcingOBL % next => next % buoyancyForcingOBL
         else
           nullify(diagnostics % buoyancyForcingOBL % next)
         end if


      end subroutine mpas_create_diagnostics_links


      subroutine mpas_create_average_links(average, prev, next)

         implicit none
         type (average_type), pointer :: average
         type (average_type), pointer, optional :: prev, next
         average % avgSsh % sendList => average % avgSsh % block % parinfo % cellsToSend
         average % avgSsh % recvList => average % avgSsh % block % parinfo % cellsToRecv
         average % avgSsh % copyList => average % avgSsh % block % parinfo % cellsToCopy
         if(present(prev)) then
           average % avgSsh % prev => prev % avgSsh
         else
           nullify(average % avgSsh % prev)
         end if
         if(present(next)) then
           average % avgSsh % next => next % avgSsh
         else
           nullify(average % avgSsh % next)
         end if


         average % varSsh % sendList => average % varSsh % block % parinfo % cellsToSend
         average % varSsh % recvList => average % varSsh % block % parinfo % cellsToRecv
         average % varSsh % copyList => average % varSsh % block % parinfo % cellsToCopy
         if(present(prev)) then
           average % varSsh % prev => prev % varSsh
         else
           nullify(average % varSsh % prev)
         end if
         if(present(next)) then
           average % varSsh % next => next % varSsh
         else
           nullify(average % varSsh % next)
         end if


         average % avgVelocityZonal % sendList => average % avgVelocityZonal % block % parinfo % cellsToSend
         average % avgVelocityZonal % recvList => average % avgVelocityZonal % block % parinfo % cellsToRecv
         average % avgVelocityZonal % copyList => average % avgVelocityZonal % block % parinfo % cellsToCopy
         if(present(prev)) then
           average % avgVelocityZonal % prev => prev % avgVelocityZonal
         else
           nullify(average % avgVelocityZonal % prev)
         end if
         if(present(next)) then
           average % avgVelocityZonal % next => next % avgVelocityZonal
         else
           nullify(average % avgVelocityZonal % next)
         end if


         average % avgVelocityMeridional % sendList => average % avgVelocityMeridional % block % parinfo % cellsToSend
         average % avgVelocityMeridional % recvList => average % avgVelocityMeridional % block % parinfo % cellsToRecv
         average % avgVelocityMeridional % copyList => average % avgVelocityMeridional % block % parinfo % cellsToCopy
         if(present(prev)) then
           average % avgVelocityMeridional % prev => prev % avgVelocityMeridional
         else
           nullify(average % avgVelocityMeridional % prev)
         end if
         if(present(next)) then
           average % avgVelocityMeridional % next => next % avgVelocityMeridional
         else
           nullify(average % avgVelocityMeridional % next)
         end if


         average % varVelocityZonal % sendList => average % varVelocityZonal % block % parinfo % cellsToSend
         average % varVelocityZonal % recvList => average % varVelocityZonal % block % parinfo % cellsToRecv
         average % varVelocityZonal % copyList => average % varVelocityZonal % block % parinfo % cellsToCopy
         if(present(prev)) then
           average % varVelocityZonal % prev => prev % varVelocityZonal
         else
           nullify(average % varVelocityZonal % prev)
         end if
         if(present(next)) then
           average % varVelocityZonal % next => next % varVelocityZonal
         else
           nullify(average % varVelocityZonal % next)
         end if


         average % varVelocityMeridional % sendList => average % varVelocityMeridional % block % parinfo % cellsToSend
         average % varVelocityMeridional % recvList => average % varVelocityMeridional % block % parinfo % cellsToRecv
         average % varVelocityMeridional % copyList => average % varVelocityMeridional % block % parinfo % cellsToCopy
         if(present(prev)) then
           average % varVelocityMeridional % prev => prev % varVelocityMeridional
         else
           nullify(average % varVelocityMeridional % prev)
         end if
         if(present(next)) then
           average % varVelocityMeridional % next => next % varVelocityMeridional
         else
           nullify(average % varVelocityMeridional % next)
         end if


         average % avgNormalVelocity % sendList => average % avgNormalVelocity % block % parinfo % edgesToSend
         average % avgNormalVelocity % recvList => average % avgNormalVelocity % block % parinfo % edgesToRecv
         average % avgNormalVelocity % copyList => average % avgNormalVelocity % block % parinfo % edgesToCopy
         if(present(prev)) then
           average % avgNormalVelocity % prev => prev % avgNormalVelocity
         else
           nullify(average % avgNormalVelocity % prev)
         end if
         if(present(next)) then
           average % avgNormalVelocity % next => next % avgNormalVelocity
         else
           nullify(average % avgNormalVelocity % next)
         end if


         average % varNormalVelocity % sendList => average % varNormalVelocity % block % parinfo % edgesToSend
         average % varNormalVelocity % recvList => average % varNormalVelocity % block % parinfo % edgesToRecv
         average % varNormalVelocity % copyList => average % varNormalVelocity % block % parinfo % edgesToCopy
         if(present(prev)) then
           average % varNormalVelocity % prev => prev % varNormalVelocity
         else
           nullify(average % varNormalVelocity % prev)
         end if
         if(present(next)) then
           average % varNormalVelocity % next => next % varNormalVelocity
         else
           nullify(average % varNormalVelocity % next)
         end if


         average % avgVertVelocityTop % sendList => average % avgVertVelocityTop % block % parinfo % cellsToSend
         average % avgVertVelocityTop % recvList => average % avgVertVelocityTop % block % parinfo % cellsToRecv
         average % avgVertVelocityTop % copyList => average % avgVertVelocityTop % block % parinfo % cellsToCopy
         if(present(prev)) then
           average % avgVertVelocityTop % prev => prev % avgVertVelocityTop
         else
           nullify(average % avgVertVelocityTop % prev)
         end if
         if(present(next)) then
           average % avgVertVelocityTop % next => next % avgVertVelocityTop
         else
           nullify(average % avgVertVelocityTop % next)
         end if


      end subroutine mpas_create_average_links


      subroutine mpas_create_forcing_links(forcing, prev, next)

         implicit none
         type (forcing_type), pointer :: forcing
         type (forcing_type), pointer, optional :: prev, next
         forcing % surfaceTracerFlux % sendList => forcing % surfaceTracerFlux % block % parinfo % cellsToSend
         forcing % surfaceTracerFlux % recvList => forcing % surfaceTracerFlux % block % parinfo % cellsToRecv
         forcing % surfaceTracerFlux % copyList => forcing % surfaceTracerFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % surfaceTracerFlux % prev => prev % surfaceTracerFlux
         else
           nullify(forcing % surfaceTracerFlux % prev)
         end if
         if(present(next)) then
           forcing % surfaceTracerFlux % next => next % surfaceTracerFlux
         else
           nullify(forcing % surfaceTracerFlux % next)
         end if


         forcing % avgTracersSurfaceValue % sendList => forcing % avgTracersSurfaceValue % block % parinfo % cellsToSend
         forcing % avgTracersSurfaceValue % recvList => forcing % avgTracersSurfaceValue % block % parinfo % cellsToRecv
         forcing % avgTracersSurfaceValue % copyList => forcing % avgTracersSurfaceValue % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % avgTracersSurfaceValue % prev => prev % avgTracersSurfaceValue
         else
           nullify(forcing % avgTracersSurfaceValue % prev)
         end if
         if(present(next)) then
           forcing % avgTracersSurfaceValue % next => next % avgTracersSurfaceValue
         else
           nullify(forcing % avgTracersSurfaceValue % next)
         end if


         forcing % avgSurfaceVelocity % sendList => forcing % avgSurfaceVelocity % block % parinfo % cellsToSend
         forcing % avgSurfaceVelocity % recvList => forcing % avgSurfaceVelocity % block % parinfo % cellsToRecv
         forcing % avgSurfaceVelocity % copyList => forcing % avgSurfaceVelocity % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % avgSurfaceVelocity % prev => prev % avgSurfaceVelocity
         else
           nullify(forcing % avgSurfaceVelocity % prev)
         end if
         if(present(next)) then
           forcing % avgSurfaceVelocity % next => next % avgSurfaceVelocity
         else
           nullify(forcing % avgSurfaceVelocity % next)
         end if


         forcing % avgSSHGradient % sendList => forcing % avgSSHGradient % block % parinfo % cellsToSend
         forcing % avgSSHGradient % recvList => forcing % avgSSHGradient % block % parinfo % cellsToRecv
         forcing % avgSSHGradient % copyList => forcing % avgSSHGradient % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % avgSSHGradient % prev => prev % avgSSHGradient
         else
           nullify(forcing % avgSSHGradient % prev)
         end if
         if(present(next)) then
           forcing % avgSSHGradient % next => next % avgSSHGradient
         else
           nullify(forcing % avgSSHGradient % next)
         end if


         forcing % surfaceWindStress % sendList => forcing % surfaceWindStress % block % parinfo % edgesToSend
         forcing % surfaceWindStress % recvList => forcing % surfaceWindStress % block % parinfo % edgesToRecv
         forcing % surfaceWindStress % copyList => forcing % surfaceWindStress % block % parinfo % edgesToCopy
         if(present(prev)) then
           forcing % surfaceWindStress % prev => prev % surfaceWindStress
         else
           nullify(forcing % surfaceWindStress % prev)
         end if
         if(present(next)) then
           forcing % surfaceWindStress % next => next % surfaceWindStress
         else
           nullify(forcing % surfaceWindStress % next)
         end if


         forcing % surfaceWindStressMagnitude % sendList => forcing % surfaceWindStressMagnitude % block % parinfo % cellsToSend
         forcing % surfaceWindStressMagnitude % recvList => forcing % surfaceWindStressMagnitude % block % parinfo % cellsToRecv
         forcing % surfaceWindStressMagnitude % copyList => forcing % surfaceWindStressMagnitude % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % surfaceWindStressMagnitude % prev => prev % surfaceWindStressMagnitude
         else
           nullify(forcing % surfaceWindStressMagnitude % prev)
         end if
         if(present(next)) then
           forcing % surfaceWindStressMagnitude % next => next % surfaceWindStressMagnitude
         else
           nullify(forcing % surfaceWindStressMagnitude % next)
         end if


         forcing % surfaceMassFlux % sendList => forcing % surfaceMassFlux % block % parinfo % cellsToSend
         forcing % surfaceMassFlux % recvList => forcing % surfaceMassFlux % block % parinfo % cellsToRecv
         forcing % surfaceMassFlux % copyList => forcing % surfaceMassFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % surfaceMassFlux % prev => prev % surfaceMassFlux
         else
           nullify(forcing % surfaceMassFlux % prev)
         end if
         if(present(next)) then
           forcing % surfaceMassFlux % next => next % surfaceMassFlux
         else
           nullify(forcing % surfaceMassFlux % next)
         end if


         forcing % seaSurfacePressure % sendList => forcing % seaSurfacePressure % block % parinfo % cellsToSend
         forcing % seaSurfacePressure % recvList => forcing % seaSurfacePressure % block % parinfo % cellsToRecv
         forcing % seaSurfacePressure % copyList => forcing % seaSurfacePressure % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % seaSurfacePressure % prev => prev % seaSurfacePressure
         else
           nullify(forcing % seaSurfacePressure % prev)
         end if
         if(present(next)) then
           forcing % seaSurfacePressure % next => next % seaSurfacePressure
         else
           nullify(forcing % seaSurfacePressure % next)
         end if


         forcing % seaIceEnergy % sendList => forcing % seaIceEnergy % block % parinfo % cellsToSend
         forcing % seaIceEnergy % recvList => forcing % seaIceEnergy % block % parinfo % cellsToRecv
         forcing % seaIceEnergy % copyList => forcing % seaIceEnergy % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % seaIceEnergy % prev => prev % seaIceEnergy
         else
           nullify(forcing % seaIceEnergy % prev)
         end if
         if(present(next)) then
           forcing % seaIceEnergy % next => next % seaIceEnergy
         else
           nullify(forcing % seaIceEnergy % next)
         end if


         forcing % penetrativeTemperatureFlux % sendList => forcing % penetrativeTemperatureFlux % block % parinfo % cellsToSend
         forcing % penetrativeTemperatureFlux % recvList => forcing % penetrativeTemperatureFlux % block % parinfo % cellsToRecv
         forcing % penetrativeTemperatureFlux % copyList => forcing % penetrativeTemperatureFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % penetrativeTemperatureFlux % prev => prev % penetrativeTemperatureFlux
         else
           nullify(forcing % penetrativeTemperatureFlux % prev)
         end if
         if(present(next)) then
           forcing % penetrativeTemperatureFlux % next => next % penetrativeTemperatureFlux
         else
           nullify(forcing % penetrativeTemperatureFlux % next)
         end if


         forcing % transmissionCoefficients % sendList => forcing % transmissionCoefficients % block % parinfo % cellsToSend
         forcing % transmissionCoefficients % recvList => forcing % transmissionCoefficients % block % parinfo % cellsToRecv
         forcing % transmissionCoefficients % copyList => forcing % transmissionCoefficients % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % transmissionCoefficients % prev => prev % transmissionCoefficients
         else
           nullify(forcing % transmissionCoefficients % prev)
         end if
         if(present(next)) then
           forcing % transmissionCoefficients % next => next % transmissionCoefficients
         else
           nullify(forcing % transmissionCoefficients % next)
         end if


         forcing % windStressZonal % sendList => forcing % windStressZonal % block % parinfo % cellsToSend
         forcing % windStressZonal % recvList => forcing % windStressZonal % block % parinfo % cellsToRecv
         forcing % windStressZonal % copyList => forcing % windStressZonal % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % windStressZonal % prev => prev % windStressZonal
         else
           nullify(forcing % windStressZonal % prev)
         end if
         if(present(next)) then
           forcing % windStressZonal % next => next % windStressZonal
         else
           nullify(forcing % windStressZonal % next)
         end if


         forcing % windStressMeridional % sendList => forcing % windStressMeridional % block % parinfo % cellsToSend
         forcing % windStressMeridional % recvList => forcing % windStressMeridional % block % parinfo % cellsToRecv
         forcing % windStressMeridional % copyList => forcing % windStressMeridional % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % windStressMeridional % prev => prev % windStressMeridional
         else
           nullify(forcing % windStressMeridional % prev)
         end if
         if(present(next)) then
           forcing % windStressMeridional % next => next % windStressMeridional
         else
           nullify(forcing % windStressMeridional % next)
         end if


         forcing % latentHeatFlux % sendList => forcing % latentHeatFlux % block % parinfo % cellsToSend
         forcing % latentHeatFlux % recvList => forcing % latentHeatFlux % block % parinfo % cellsToRecv
         forcing % latentHeatFlux % copyList => forcing % latentHeatFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % latentHeatFlux % prev => prev % latentHeatFlux
         else
           nullify(forcing % latentHeatFlux % prev)
         end if
         if(present(next)) then
           forcing % latentHeatFlux % next => next % latentHeatFlux
         else
           nullify(forcing % latentHeatFlux % next)
         end if


         forcing % sensibleHeatFlux % sendList => forcing % sensibleHeatFlux % block % parinfo % cellsToSend
         forcing % sensibleHeatFlux % recvList => forcing % sensibleHeatFlux % block % parinfo % cellsToRecv
         forcing % sensibleHeatFlux % copyList => forcing % sensibleHeatFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % sensibleHeatFlux % prev => prev % sensibleHeatFlux
         else
           nullify(forcing % sensibleHeatFlux % prev)
         end if
         if(present(next)) then
           forcing % sensibleHeatFlux % next => next % sensibleHeatFlux
         else
           nullify(forcing % sensibleHeatFlux % next)
         end if


         forcing % longWaveHeatFluxUp % sendList => forcing % longWaveHeatFluxUp % block % parinfo % cellsToSend
         forcing % longWaveHeatFluxUp % recvList => forcing % longWaveHeatFluxUp % block % parinfo % cellsToRecv
         forcing % longWaveHeatFluxUp % copyList => forcing % longWaveHeatFluxUp % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % longWaveHeatFluxUp % prev => prev % longWaveHeatFluxUp
         else
           nullify(forcing % longWaveHeatFluxUp % prev)
         end if
         if(present(next)) then
           forcing % longWaveHeatFluxUp % next => next % longWaveHeatFluxUp
         else
           nullify(forcing % longWaveHeatFluxUp % next)
         end if


         forcing % longWaveHeatFluxDown % sendList => forcing % longWaveHeatFluxDown % block % parinfo % cellsToSend
         forcing % longWaveHeatFluxDown % recvList => forcing % longWaveHeatFluxDown % block % parinfo % cellsToRecv
         forcing % longWaveHeatFluxDown % copyList => forcing % longWaveHeatFluxDown % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % longWaveHeatFluxDown % prev => prev % longWaveHeatFluxDown
         else
           nullify(forcing % longWaveHeatFluxDown % prev)
         end if
         if(present(next)) then
           forcing % longWaveHeatFluxDown % next => next % longWaveHeatFluxDown
         else
           nullify(forcing % longWaveHeatFluxDown % next)
         end if


         forcing % seaIceHeatFlux % sendList => forcing % seaIceHeatFlux % block % parinfo % cellsToSend
         forcing % seaIceHeatFlux % recvList => forcing % seaIceHeatFlux % block % parinfo % cellsToRecv
         forcing % seaIceHeatFlux % copyList => forcing % seaIceHeatFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % seaIceHeatFlux % prev => prev % seaIceHeatFlux
         else
           nullify(forcing % seaIceHeatFlux % prev)
         end if
         if(present(next)) then
           forcing % seaIceHeatFlux % next => next % seaIceHeatFlux
         else
           nullify(forcing % seaIceHeatFlux % next)
         end if


         forcing % shortWaveHeatFlux % sendList => forcing % shortWaveHeatFlux % block % parinfo % cellsToSend
         forcing % shortWaveHeatFlux % recvList => forcing % shortWaveHeatFlux % block % parinfo % cellsToRecv
         forcing % shortWaveHeatFlux % copyList => forcing % shortWaveHeatFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % shortWaveHeatFlux % prev => prev % shortWaveHeatFlux
         else
           nullify(forcing % shortWaveHeatFlux % prev)
         end if
         if(present(next)) then
           forcing % shortWaveHeatFlux % next => next % shortWaveHeatFlux
         else
           nullify(forcing % shortWaveHeatFlux % next)
         end if


         forcing % evaporationFlux % sendList => forcing % evaporationFlux % block % parinfo % cellsToSend
         forcing % evaporationFlux % recvList => forcing % evaporationFlux % block % parinfo % cellsToRecv
         forcing % evaporationFlux % copyList => forcing % evaporationFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % evaporationFlux % prev => prev % evaporationFlux
         else
           nullify(forcing % evaporationFlux % prev)
         end if
         if(present(next)) then
           forcing % evaporationFlux % next => next % evaporationFlux
         else
           nullify(forcing % evaporationFlux % next)
         end if


         forcing % seaIceSalinityFlux % sendList => forcing % seaIceSalinityFlux % block % parinfo % cellsToSend
         forcing % seaIceSalinityFlux % recvList => forcing % seaIceSalinityFlux % block % parinfo % cellsToRecv
         forcing % seaIceSalinityFlux % copyList => forcing % seaIceSalinityFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % seaIceSalinityFlux % prev => prev % seaIceSalinityFlux
         else
           nullify(forcing % seaIceSalinityFlux % prev)
         end if
         if(present(next)) then
           forcing % seaIceSalinityFlux % next => next % seaIceSalinityFlux
         else
           nullify(forcing % seaIceSalinityFlux % next)
         end if


         forcing % seaIceFreshWaterFlux % sendList => forcing % seaIceFreshWaterFlux % block % parinfo % cellsToSend
         forcing % seaIceFreshWaterFlux % recvList => forcing % seaIceFreshWaterFlux % block % parinfo % cellsToRecv
         forcing % seaIceFreshWaterFlux % copyList => forcing % seaIceFreshWaterFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % seaIceFreshWaterFlux % prev => prev % seaIceFreshWaterFlux
         else
           nullify(forcing % seaIceFreshWaterFlux % prev)
         end if
         if(present(next)) then
           forcing % seaIceFreshWaterFlux % next => next % seaIceFreshWaterFlux
         else
           nullify(forcing % seaIceFreshWaterFlux % next)
         end if


         forcing % riverRunoffFlux % sendList => forcing % riverRunoffFlux % block % parinfo % cellsToSend
         forcing % riverRunoffFlux % recvList => forcing % riverRunoffFlux % block % parinfo % cellsToRecv
         forcing % riverRunoffFlux % copyList => forcing % riverRunoffFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % riverRunoffFlux % prev => prev % riverRunoffFlux
         else
           nullify(forcing % riverRunoffFlux % prev)
         end if
         if(present(next)) then
           forcing % riverRunoffFlux % next => next % riverRunoffFlux
         else
           nullify(forcing % riverRunoffFlux % next)
         end if


         forcing % iceRunoffFlux % sendList => forcing % iceRunoffFlux % block % parinfo % cellsToSend
         forcing % iceRunoffFlux % recvList => forcing % iceRunoffFlux % block % parinfo % cellsToRecv
         forcing % iceRunoffFlux % copyList => forcing % iceRunoffFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % iceRunoffFlux % prev => prev % iceRunoffFlux
         else
           nullify(forcing % iceRunoffFlux % prev)
         end if
         if(present(next)) then
           forcing % iceRunoffFlux % next => next % iceRunoffFlux
         else
           nullify(forcing % iceRunoffFlux % next)
         end if


         forcing % rainFlux % sendList => forcing % rainFlux % block % parinfo % cellsToSend
         forcing % rainFlux % recvList => forcing % rainFlux % block % parinfo % cellsToRecv
         forcing % rainFlux % copyList => forcing % rainFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % rainFlux % prev => prev % rainFlux
         else
           nullify(forcing % rainFlux % prev)
         end if
         if(present(next)) then
           forcing % rainFlux % next => next % rainFlux
         else
           nullify(forcing % rainFlux % next)
         end if


         forcing % snowFlux % sendList => forcing % snowFlux % block % parinfo % cellsToSend
         forcing % snowFlux % recvList => forcing % snowFlux % block % parinfo % cellsToRecv
         forcing % snowFlux % copyList => forcing % snowFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % snowFlux % prev => prev % snowFlux
         else
           nullify(forcing % snowFlux % prev)
         end if
         if(present(next)) then
           forcing % snowFlux % next => next % snowFlux
         else
           nullify(forcing % snowFlux % next)
         end if


         forcing % iceFraction % sendList => forcing % iceFraction % block % parinfo % cellsToSend
         forcing % iceFraction % recvList => forcing % iceFraction % block % parinfo % cellsToRecv
         forcing % iceFraction % copyList => forcing % iceFraction % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % iceFraction % prev => prev % iceFraction
         else
           nullify(forcing % iceFraction % prev)
         end if
         if(present(next)) then
           forcing % iceFraction % next => next % iceFraction
         else
           nullify(forcing % iceFraction % next)
         end if


         forcing % prognosticCO2 % sendList => forcing % prognosticCO2 % block % parinfo % cellsToSend
         forcing % prognosticCO2 % recvList => forcing % prognosticCO2 % block % parinfo % cellsToRecv
         forcing % prognosticCO2 % copyList => forcing % prognosticCO2 % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % prognosticCO2 % prev => prev % prognosticCO2
         else
           nullify(forcing % prognosticCO2 % prev)
         end if
         if(present(next)) then
           forcing % prognosticCO2 % next => next % prognosticCO2
         else
           nullify(forcing % prognosticCO2 % next)
         end if


         forcing % diagnosticCO2 % sendList => forcing % diagnosticCO2 % block % parinfo % cellsToSend
         forcing % diagnosticCO2 % recvList => forcing % diagnosticCO2 % block % parinfo % cellsToRecv
         forcing % diagnosticCO2 % copyList => forcing % diagnosticCO2 % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % diagnosticCO2 % prev => prev % diagnosticCO2
         else
           nullify(forcing % diagnosticCO2 % prev)
         end if
         if(present(next)) then
           forcing % diagnosticCO2 % next => next % diagnosticCO2
         else
           nullify(forcing % diagnosticCO2 % next)
         end if


         forcing % squaredWindSpeed10Meter % sendList => forcing % squaredWindSpeed10Meter % block % parinfo % cellsToSend
         forcing % squaredWindSpeed10Meter % recvList => forcing % squaredWindSpeed10Meter % block % parinfo % cellsToRecv
         forcing % squaredWindSpeed10Meter % copyList => forcing % squaredWindSpeed10Meter % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % squaredWindSpeed10Meter % prev => prev % squaredWindSpeed10Meter
         else
           nullify(forcing % squaredWindSpeed10Meter % prev)
         end if
         if(present(next)) then
           forcing % squaredWindSpeed10Meter % next => next % squaredWindSpeed10Meter
         else
           nullify(forcing % squaredWindSpeed10Meter % next)
         end if


         forcing % CO2Flux % sendList => forcing % CO2Flux % block % parinfo % cellsToSend
         forcing % CO2Flux % recvList => forcing % CO2Flux % block % parinfo % cellsToRecv
         forcing % CO2Flux % copyList => forcing % CO2Flux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % CO2Flux % prev => prev % CO2Flux
         else
           nullify(forcing % CO2Flux % prev)
         end if
         if(present(next)) then
           forcing % CO2Flux % next => next % CO2Flux
         else
           nullify(forcing % CO2Flux % next)
         end if


         forcing % DMSFlux % sendList => forcing % DMSFlux % block % parinfo % cellsToSend
         forcing % DMSFlux % recvList => forcing % DMSFlux % block % parinfo % cellsToRecv
         forcing % DMSFlux % copyList => forcing % DMSFlux % block % parinfo % cellsToCopy
         if(present(prev)) then
           forcing % DMSFlux % prev => prev % DMSFlux
         else
           nullify(forcing % DMSFlux % prev)
         end if
         if(present(next)) then
           forcing % DMSFlux % next => next % DMSFlux
         else
           nullify(forcing % DMSFlux % next)
         end if


      end subroutine mpas_create_forcing_links


      subroutine mpas_create_scratch_links(scratch, prev, next)

         implicit none
         type (scratch_type), pointer :: scratch
         type (scratch_type), pointer, optional :: prev, next
         scratch % vorticityGradientTangentialComponent % sendList => scratch % vorticityGradientTangentialComponent % block % &
parinfo % edgesToSend
         scratch % vorticityGradientTangentialComponent % recvList => scratch % vorticityGradientTangentialComponent % block % &
parinfo % edgesToRecv
         scratch % vorticityGradientTangentialComponent % copyList => scratch % vorticityGradientTangentialComponent % block % &
parinfo % edgesToCopy
         if(present(prev)) then
           scratch % vorticityGradientTangentialComponent % prev => prev % vorticityGradientTangentialComponent
         else
           nullify(scratch % vorticityGradientTangentialComponent % prev)
         end if
         if(present(next)) then
           scratch % vorticityGradientTangentialComponent % next => next % vorticityGradientTangentialComponent
         else
           nullify(scratch % vorticityGradientTangentialComponent % next)
         end if


         scratch % vorticityGradientNormalComponent % sendList => scratch % vorticityGradientNormalComponent % block % parinfo % &
edgesToSend
         scratch % vorticityGradientNormalComponent % recvList => scratch % vorticityGradientNormalComponent % block % parinfo % &
edgesToRecv
         scratch % vorticityGradientNormalComponent % copyList => scratch % vorticityGradientNormalComponent % block % parinfo % &
edgesToCopy
         if(present(prev)) then
           scratch % vorticityGradientNormalComponent % prev => prev % vorticityGradientNormalComponent
         else
           nullify(scratch % vorticityGradientNormalComponent % prev)
         end if
         if(present(next)) then
           scratch % vorticityGradientNormalComponent % next => next % vorticityGradientNormalComponent
         else
           nullify(scratch % vorticityGradientNormalComponent % next)
         end if


         scratch % normalizedRelativeVorticityVertex % sendList => scratch % normalizedRelativeVorticityVertex % block % parinfo % &
verticesToSend
         scratch % normalizedRelativeVorticityVertex % recvList => scratch % normalizedRelativeVorticityVertex % block % parinfo % &
verticesToRecv
         scratch % normalizedRelativeVorticityVertex % copyList => scratch % normalizedRelativeVorticityVertex % block % parinfo % &
verticesToCopy
         if(present(prev)) then
           scratch % normalizedRelativeVorticityVertex % prev => prev % normalizedRelativeVorticityVertex
         else
           nullify(scratch % normalizedRelativeVorticityVertex % prev)
         end if
         if(present(next)) then
           scratch % normalizedRelativeVorticityVertex % next => next % normalizedRelativeVorticityVertex
         else
           nullify(scratch % normalizedRelativeVorticityVertex % next)
         end if


         scratch % normalizedPlanetaryVorticityVertex % sendList => scratch % normalizedPlanetaryVorticityVertex % block % parinfo &
% verticesToSend
         scratch % normalizedPlanetaryVorticityVertex % recvList => scratch % normalizedPlanetaryVorticityVertex % block % parinfo &
% verticesToRecv
         scratch % normalizedPlanetaryVorticityVertex % copyList => scratch % normalizedPlanetaryVorticityVertex % block % parinfo &
% verticesToCopy
         if(present(prev)) then
           scratch % normalizedPlanetaryVorticityVertex % prev => prev % normalizedPlanetaryVorticityVertex
         else
           nullify(scratch % normalizedPlanetaryVorticityVertex % prev)
         end if
         if(present(next)) then
           scratch % normalizedPlanetaryVorticityVertex % next => next % normalizedPlanetaryVorticityVertex
         else
           nullify(scratch % normalizedPlanetaryVorticityVertex % next)
         end if


         scratch % kineticEnergyVertex % sendList => scratch % kineticEnergyVertex % block % parinfo % verticesToSend
         scratch % kineticEnergyVertex % recvList => scratch % kineticEnergyVertex % block % parinfo % verticesToRecv
         scratch % kineticEnergyVertex % copyList => scratch % kineticEnergyVertex % block % parinfo % verticesToCopy
         if(present(prev)) then
           scratch % kineticEnergyVertex % prev => prev % kineticEnergyVertex
         else
           nullify(scratch % kineticEnergyVertex % prev)
         end if
         if(present(next)) then
           scratch % kineticEnergyVertex % next => next % kineticEnergyVertex
         else
           nullify(scratch % kineticEnergyVertex % next)
         end if


         scratch % kineticEnergyVertexOnCells % sendList => scratch % kineticEnergyVertexOnCells % block % parinfo % cellsToSend
         scratch % kineticEnergyVertexOnCells % recvList => scratch % kineticEnergyVertexOnCells % block % parinfo % cellsToRecv
         scratch % kineticEnergyVertexOnCells % copyList => scratch % kineticEnergyVertexOnCells % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % kineticEnergyVertexOnCells % prev => prev % kineticEnergyVertexOnCells
         else
           nullify(scratch % kineticEnergyVertexOnCells % prev)
         end if
         if(present(next)) then
           scratch % kineticEnergyVertexOnCells % next => next % kineticEnergyVertexOnCells
         else
           nullify(scratch % kineticEnergyVertexOnCells % next)
         end if


         scratch % densitySurfaceDisplaced % sendList => scratch % densitySurfaceDisplaced % block % parinfo % cellsToSend
         scratch % densitySurfaceDisplaced % recvList => scratch % densitySurfaceDisplaced % block % parinfo % cellsToRecv
         scratch % densitySurfaceDisplaced % copyList => scratch % densitySurfaceDisplaced % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % densitySurfaceDisplaced % prev => prev % densitySurfaceDisplaced
         else
           nullify(scratch % densitySurfaceDisplaced % prev)
         end if
         if(present(next)) then
           scratch % densitySurfaceDisplaced % next => next % densitySurfaceDisplaced
         else
           nullify(scratch % densitySurfaceDisplaced % next)
         end if


         scratch % thermalExpansionCoeff % sendList => scratch % thermalExpansionCoeff % block % parinfo % cellsToSend
         scratch % thermalExpansionCoeff % recvList => scratch % thermalExpansionCoeff % block % parinfo % cellsToRecv
         scratch % thermalExpansionCoeff % copyList => scratch % thermalExpansionCoeff % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % thermalExpansionCoeff % prev => prev % thermalExpansionCoeff
         else
           nullify(scratch % thermalExpansionCoeff % prev)
         end if
         if(present(next)) then
           scratch % thermalExpansionCoeff % next => next % thermalExpansionCoeff
         else
           nullify(scratch % thermalExpansionCoeff % next)
         end if


         scratch % salineContractionCoeff % sendList => scratch % salineContractionCoeff % block % parinfo % cellsToSend
         scratch % salineContractionCoeff % recvList => scratch % salineContractionCoeff % block % parinfo % cellsToRecv
         scratch % salineContractionCoeff % copyList => scratch % salineContractionCoeff % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % salineContractionCoeff % prev => prev % salineContractionCoeff
         else
           nullify(scratch % salineContractionCoeff % prev)
         end if
         if(present(next)) then
           scratch % salineContractionCoeff % next => next % salineContractionCoeff
         else
           nullify(scratch % salineContractionCoeff % next)
         end if


         scratch % normalVelocityTest % sendList => scratch % normalVelocityTest % block % parinfo % edgesToSend
         scratch % normalVelocityTest % recvList => scratch % normalVelocityTest % block % parinfo % edgesToRecv
         scratch % normalVelocityTest % copyList => scratch % normalVelocityTest % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % normalVelocityTest % prev => prev % normalVelocityTest
         else
           nullify(scratch % normalVelocityTest % prev)
         end if
         if(present(next)) then
           scratch % normalVelocityTest % next => next % normalVelocityTest
         else
           nullify(scratch % normalVelocityTest % next)
         end if


         scratch % tangentialVelocityTest % sendList => scratch % tangentialVelocityTest % block % parinfo % edgesToSend
         scratch % tangentialVelocityTest % recvList => scratch % tangentialVelocityTest % block % parinfo % edgesToRecv
         scratch % tangentialVelocityTest % copyList => scratch % tangentialVelocityTest % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % tangentialVelocityTest % prev => prev % tangentialVelocityTest
         else
           nullify(scratch % tangentialVelocityTest % prev)
         end if
         if(present(next)) then
           scratch % tangentialVelocityTest % next => next % tangentialVelocityTest
         else
           nullify(scratch % tangentialVelocityTest % next)
         end if


         scratch % strainRateR3Cell % sendList => scratch % strainRateR3Cell % block % parinfo % cellsToSend
         scratch % strainRateR3Cell % recvList => scratch % strainRateR3Cell % block % parinfo % cellsToRecv
         scratch % strainRateR3Cell % copyList => scratch % strainRateR3Cell % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % strainRateR3Cell % prev => prev % strainRateR3Cell
         else
           nullify(scratch % strainRateR3Cell % prev)
         end if
         if(present(next)) then
           scratch % strainRateR3Cell % next => next % strainRateR3Cell
         else
           nullify(scratch % strainRateR3Cell % next)
         end if


         scratch % strainRateR3CellSolution % sendList => scratch % strainRateR3CellSolution % block % parinfo % cellsToSend
         scratch % strainRateR3CellSolution % recvList => scratch % strainRateR3CellSolution % block % parinfo % cellsToRecv
         scratch % strainRateR3CellSolution % copyList => scratch % strainRateR3CellSolution % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % strainRateR3CellSolution % prev => prev % strainRateR3CellSolution
         else
           nullify(scratch % strainRateR3CellSolution % prev)
         end if
         if(present(next)) then
           scratch % strainRateR3CellSolution % next => next % strainRateR3CellSolution
         else
           nullify(scratch % strainRateR3CellSolution % next)
         end if


         scratch % strainRateR3Edge % sendList => scratch % strainRateR3Edge % block % parinfo % edgesToSend
         scratch % strainRateR3Edge % recvList => scratch % strainRateR3Edge % block % parinfo % edgesToRecv
         scratch % strainRateR3Edge % copyList => scratch % strainRateR3Edge % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % strainRateR3Edge % prev => prev % strainRateR3Edge
         else
           nullify(scratch % strainRateR3Edge % prev)
         end if
         if(present(next)) then
           scratch % strainRateR3Edge % next => next % strainRateR3Edge
         else
           nullify(scratch % strainRateR3Edge % next)
         end if


         scratch % strainRateLonLatRCell % sendList => scratch % strainRateLonLatRCell % block % parinfo % cellsToSend
         scratch % strainRateLonLatRCell % recvList => scratch % strainRateLonLatRCell % block % parinfo % cellsToRecv
         scratch % strainRateLonLatRCell % copyList => scratch % strainRateLonLatRCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % strainRateLonLatRCell % prev => prev % strainRateLonLatRCell
         else
           nullify(scratch % strainRateLonLatRCell % prev)
         end if
         if(present(next)) then
           scratch % strainRateLonLatRCell % next => next % strainRateLonLatRCell
         else
           nullify(scratch % strainRateLonLatRCell % next)
         end if


         scratch % strainRateLonLatRCellSolution % sendList => scratch % strainRateLonLatRCellSolution % block % parinfo % &
cellsToSend
         scratch % strainRateLonLatRCellSolution % recvList => scratch % strainRateLonLatRCellSolution % block % parinfo % &
cellsToRecv
         scratch % strainRateLonLatRCellSolution % copyList => scratch % strainRateLonLatRCellSolution % block % parinfo % &
cellsToCopy
         if(present(prev)) then
           scratch % strainRateLonLatRCellSolution % prev => prev % strainRateLonLatRCellSolution
         else
           nullify(scratch % strainRateLonLatRCellSolution % prev)
         end if
         if(present(next)) then
           scratch % strainRateLonLatRCellSolution % next => next % strainRateLonLatRCellSolution
         else
           nullify(scratch % strainRateLonLatRCellSolution % next)
         end if


         scratch % strainRateLonLatREdge % sendList => scratch % strainRateLonLatREdge % block % parinfo % edgesToSend
         scratch % strainRateLonLatREdge % recvList => scratch % strainRateLonLatREdge % block % parinfo % edgesToRecv
         scratch % strainRateLonLatREdge % copyList => scratch % strainRateLonLatREdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % strainRateLonLatREdge % prev => prev % strainRateLonLatREdge
         else
           nullify(scratch % strainRateLonLatREdge % prev)
         end if
         if(present(next)) then
           scratch % strainRateLonLatREdge % next => next % strainRateLonLatREdge
         else
           nullify(scratch % strainRateLonLatREdge % next)
         end if


         scratch % divTensorR3Cell % sendList => scratch % divTensorR3Cell % block % parinfo % cellsToSend
         scratch % divTensorR3Cell % recvList => scratch % divTensorR3Cell % block % parinfo % cellsToRecv
         scratch % divTensorR3Cell % copyList => scratch % divTensorR3Cell % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % divTensorR3Cell % prev => prev % divTensorR3Cell
         else
           nullify(scratch % divTensorR3Cell % prev)
         end if
         if(present(next)) then
           scratch % divTensorR3Cell % next => next % divTensorR3Cell
         else
           nullify(scratch % divTensorR3Cell % next)
         end if


         scratch % divTensorR3CellSolution % sendList => scratch % divTensorR3CellSolution % block % parinfo % cellsToSend
         scratch % divTensorR3CellSolution % recvList => scratch % divTensorR3CellSolution % block % parinfo % cellsToRecv
         scratch % divTensorR3CellSolution % copyList => scratch % divTensorR3CellSolution % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % divTensorR3CellSolution % prev => prev % divTensorR3CellSolution
         else
           nullify(scratch % divTensorR3CellSolution % prev)
         end if
         if(present(next)) then
           scratch % divTensorR3CellSolution % next => next % divTensorR3CellSolution
         else
           nullify(scratch % divTensorR3CellSolution % next)
         end if


         scratch % divTensorLonLatRCell % sendList => scratch % divTensorLonLatRCell % block % parinfo % cellsToSend
         scratch % divTensorLonLatRCell % recvList => scratch % divTensorLonLatRCell % block % parinfo % cellsToRecv
         scratch % divTensorLonLatRCell % copyList => scratch % divTensorLonLatRCell % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % divTensorLonLatRCell % prev => prev % divTensorLonLatRCell
         else
           nullify(scratch % divTensorLonLatRCell % prev)
         end if
         if(present(next)) then
           scratch % divTensorLonLatRCell % next => next % divTensorLonLatRCell
         else
           nullify(scratch % divTensorLonLatRCell % next)
         end if


         scratch % divTensorLonLatRCellSolution % sendList => scratch % divTensorLonLatRCellSolution % block % parinfo % &
cellsToSend
         scratch % divTensorLonLatRCellSolution % recvList => scratch % divTensorLonLatRCellSolution % block % parinfo % &
cellsToRecv
         scratch % divTensorLonLatRCellSolution % copyList => scratch % divTensorLonLatRCellSolution % block % parinfo % &
cellsToCopy
         if(present(prev)) then
           scratch % divTensorLonLatRCellSolution % prev => prev % divTensorLonLatRCellSolution
         else
           nullify(scratch % divTensorLonLatRCellSolution % prev)
         end if
         if(present(next)) then
           scratch % divTensorLonLatRCellSolution % next => next % divTensorLonLatRCellSolution
         else
           nullify(scratch % divTensorLonLatRCellSolution % next)
         end if


         scratch % outerProductEdge % sendList => scratch % outerProductEdge % block % parinfo % edgesToSend
         scratch % outerProductEdge % recvList => scratch % outerProductEdge % block % parinfo % edgesToRecv
         scratch % outerProductEdge % copyList => scratch % outerProductEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % outerProductEdge % prev => prev % outerProductEdge
         else
           nullify(scratch % outerProductEdge % prev)
         end if
         if(present(next)) then
           scratch % outerProductEdge % next => next % outerProductEdge
         else
           nullify(scratch % outerProductEdge % next)
         end if


         scratch % normalVectorEdge % sendList => scratch % normalVectorEdge % block % parinfo % edgesToSend
         scratch % normalVectorEdge % recvList => scratch % normalVectorEdge % block % parinfo % edgesToRecv
         scratch % normalVectorEdge % copyList => scratch % normalVectorEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % normalVectorEdge % prev => prev % normalVectorEdge
         else
           nullify(scratch % normalVectorEdge % prev)
         end if
         if(present(next)) then
           scratch % normalVectorEdge % next => next % normalVectorEdge
         else
           nullify(scratch % normalVectorEdge % next)
         end if


         scratch % tangentialVectorEdge % sendList => scratch % tangentialVectorEdge % block % parinfo % edgesToSend
         scratch % tangentialVectorEdge % recvList => scratch % tangentialVectorEdge % block % parinfo % edgesToRecv
         scratch % tangentialVectorEdge % copyList => scratch % tangentialVectorEdge % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % tangentialVectorEdge % prev => prev % tangentialVectorEdge
         else
           nullify(scratch % tangentialVectorEdge % prev)
         end if
         if(present(next)) then
           scratch % tangentialVectorEdge % next => next % tangentialVectorEdge
         else
           nullify(scratch % tangentialVectorEdge % next)
         end if


         scratch % windStressFull % sendList => scratch % windStressFull % block % parinfo % edgesToSend
         scratch % windStressFull % recvList => scratch % windStressFull % block % parinfo % edgesToRecv
         scratch % windStressFull % copyList => scratch % windStressFull % block % parinfo % edgesToCopy
         if(present(prev)) then
           scratch % windStressFull % prev => prev % windStressFull
         else
           nullify(scratch % windStressFull % prev)
         end if
         if(present(next)) then
           scratch % windStressFull % next => next % windStressFull
         else
           nullify(scratch % windStressFull % next)
         end if


         scratch % windStressX % sendList => scratch % windStressX % block % parinfo % cellsToSend
         scratch % windStressX % recvList => scratch % windStressX % block % parinfo % cellsToRecv
         scratch % windStressX % copyList => scratch % windStressX % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % windStressX % prev => prev % windStressX
         else
           nullify(scratch % windStressX % prev)
         end if
         if(present(next)) then
           scratch % windStressX % next => next % windStressX
         else
           nullify(scratch % windStressX % next)
         end if


         scratch % windStressY % sendList => scratch % windStressY % block % parinfo % cellsToSend
         scratch % windStressY % recvList => scratch % windStressY % block % parinfo % cellsToRecv
         scratch % windStressY % copyList => scratch % windStressY % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % windStressY % prev => prev % windStressY
         else
           nullify(scratch % windStressY % prev)
         end if
         if(present(next)) then
           scratch % windStressY % next => next % windStressY
         else
           nullify(scratch % windStressY % next)
         end if


         scratch % windStressZ % sendList => scratch % windStressZ % block % parinfo % cellsToSend
         scratch % windStressZ % recvList => scratch % windStressZ % block % parinfo % cellsToRecv
         scratch % windStressZ % copyList => scratch % windStressZ % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % windStressZ % prev => prev % windStressZ
         else
           nullify(scratch % windStressZ % prev)
         end if
         if(present(next)) then
           scratch % windStressZ % next => next % windStressZ
         else
           nullify(scratch % windStressZ % next)
         end if


         scratch % windStressZonal % sendList => scratch % windStressZonal % block % parinfo % cellsToSend
         scratch % windStressZonal % recvList => scratch % windStressZonal % block % parinfo % cellsToRecv
         scratch % windStressZonal % copyList => scratch % windStressZonal % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % windStressZonal % prev => prev % windStressZonal
         else
           nullify(scratch % windStressZonal % prev)
         end if
         if(present(next)) then
           scratch % windStressZonal % next => next % windStressZonal
         else
           nullify(scratch % windStressZonal % next)
         end if


         scratch % windStressMeridional % sendList => scratch % windStressMeridional % block % parinfo % cellsToSend
         scratch % windStressMeridional % recvList => scratch % windStressMeridional % block % parinfo % cellsToRecv
         scratch % windStressMeridional % copyList => scratch % windStressMeridional % block % parinfo % cellsToCopy
         if(present(prev)) then
           scratch % windStressMeridional % prev => prev % windStressMeridional
         else
           nullify(scratch % windStressMeridional % prev)
         end if
         if(present(next)) then
           scratch % windStressMeridional % next => next % windStressMeridional
         else
           nullify(scratch % windStressMeridional % next)
         end if


      end subroutine mpas_create_scratch_links




end module mpas_grid_types

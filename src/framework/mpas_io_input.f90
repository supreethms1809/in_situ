












! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_io_input

   use mpas_grid_types
   use mpas_dmpar
   use mpas_block_decomp
   use mpas_block_creator
   use mpas_sort
   use mpas_configure
   use mpas_timekeeping
   use mpas_io_streams
   use mpas_io_units

   integer, parameter :: STREAM_INPUT=1, STREAM_SFC=2, STREAM_RESTART=3

   type io_input_object
      character (len=StrKIND) :: filename
      integer :: rd_ncid
      integer :: stream

      integer :: time

      type (MPAS_Stream_type) :: io_stream

   end type io_input_object

   integer :: readCellStart, readCellEnd, nReadCells
   integer :: readEdgeStart, readEdgeEnd, nReadEdges
   integer :: readVertexStart, readVertexEnd, nReadVertices

   contains

   subroutine mpas_input_state_for_domain(domain)!{{{

      implicit none
   
      type (domain_type), pointer :: domain

      type (block_type), pointer :: block_ptr
      type (block_type), pointer :: readingBlock
   
      integer :: i, j, k
      type (io_input_object) :: input_obj
      integer :: nCells
      integer :: nEdges
      integer :: maxEdges
      integer :: maxEdges2
      integer :: nVertices
      integer :: vertexDegree
      integer :: nVertLevels

      character (len=StrKIND) :: c_on_a_sphere
      real (kind=RKIND) :: r_sphere_radius

      integer :: ierr
      integer, dimension(:), pointer :: readIndices
      type (MPAS_IO_Handle_type) :: inputHandle
   
      type (field1dInteger), pointer :: indexToCellIDField
      type (field1dInteger), pointer :: indexToEdgeIDField
      type (field1dInteger), pointer :: indexToVertexIDField
      type (field1dInteger), pointer :: nEdgesOnCellField
      type (field2dInteger), pointer :: cellsOnCellField
      type (field2dInteger), pointer :: edgesOnCellField
      type (field2dInteger), pointer :: verticesOnCellField
      type (field2dInteger), pointer :: cellsOnEdgeField
      type (field2dInteger), pointer :: cellsOnVertexField

      type (field1dReal), pointer :: xCellField,   yCellField,   zCellField
      type (field1dReal), pointer :: xEdgeField,   yEdgeField,   zEdgeField
      type (field1dReal), pointer :: xVertexField, yVertexField, zVertexField

      type (field1DChar) :: xtime

      type (field1dInteger), pointer :: nCellsSolveField
      type (field1dInteger), pointer :: nVerticesSolveField
      type (field1dInteger), pointer :: nEdgesSolveField

      type (field1DInteger), pointer :: indexToCellID_Block
      type (field1DInteger), pointer :: nEdgesOnCell_Block
      type (field2DInteger), pointer :: cellsOnCell_Block
      type (field2DInteger), pointer :: verticesOnCell_Block
      type (field2DInteger), pointer :: edgesOnCell_Block

      type (field1DInteger), pointer :: indexToVertexID_Block
      type (field2DInteger), pointer :: cellsOnVertex_Block

      type (field1DInteger), pointer :: indexToEdgeID_Block
      type (field2DInteger), pointer :: cellsOnEdge_Block

      type (field1DReal), pointer :: xCell, yCell, zCell
      type (field1DReal), pointer :: xEdge, yEdge, zEdge
      type (field1DReal), pointer :: xVertex, yVertex, zVertex
   
      integer, dimension(:), pointer :: local_cell_list
      integer, dimension(:), pointer :: block_id, block_start, block_count
      type (graph) :: partial_global_graph_info

      type (MPAS_Time_type) :: startTime
      character(len=StrKIND) :: timeStamp, restartTimeStamp
      character(len=StrKIND) :: filename

      integer :: nHalos

      nHalos = config_num_halos

      if (config_do_restart) then
        ! this get followed by set is to ensure that the time is in standard format
        if(trim(config_start_time) == 'file') then
          open(22,file=trim(config_restart_timestamp_name),form='formatted',status='old')
          read(22,*) restartTimeStamp
          close(22)

        else
          restartTimeStamp = config_start_time
        end if

        write(stderrUnit,*) 'RestartTimeStamp ', trim(restartTimeStamp)
        call mpas_set_time(curr_time=startTime, dateTimeString=restartTimeStamp)
        call mpas_get_time(curr_time=startTime, dateTimeString=timeStamp)
        call mpas_insert_string_suffix(trim(config_restart_name), timeStamp, filename)

        input_obj % filename = trim(filename)
        input_obj % stream = STREAM_RESTART
      else
        input_obj % filename = trim(config_input_name)
        input_obj % stream = STREAM_INPUT
      end if
      inputHandle = MPAS_io_open(trim(input_obj % filename), MPAS_IO_READ, MPAS_IO_PNETCDF, ierr)
      if (ierr /= MPAS_IO_NOERR) then
        write(stderrUnit,*) ' '
        if (input_obj % stream == STREAM_RESTART) then
          write(stderrUnit,*) 'Error opening restart file ''', trim(input_obj % filename), ''''
        else if (input_obj % stream == STREAM_INPUT) then
          write(stderrUnit,*) 'Error opening input file ''', trim(input_obj % filename), ''''
        else if (input_obj % stream == STREAM_SFC) then
          write(stderrUnit,*) 'Error opening sfc file ''', trim(input_obj % filename), ''''
        end if
        write(stderrUnit,*) ' '
        call mpas_dmpar_abort(domain % dminfo)
      end if

      !
      ! Read global number of cells/edges/vertices
      !
      call MPAS_io_inq_dim(inputHandle, 'nCells', nCells, ierr)
      call MPAS_io_inq_dim(inputHandle, 'nEdges', nEdges, ierr)
      call MPAS_io_inq_dim(inputHandle, 'maxEdges', maxEdges, ierr)
      call MPAS_io_inq_dim(inputHandle, 'maxEdges2', maxEdges2, ierr)
      call MPAS_io_inq_dim(inputHandle, 'nVertices', nVertices, ierr)
      call MPAS_io_inq_dim(inputHandle, 'vertexDegree', vertexDegree, ierr)
      call MPAS_io_inq_dim(inputHandle, 'nVertLevels', nVertLevels, ierr)
   
      !
      ! Determine the range of cells/edges/vertices that a processor will initially read
      !   from the input file
      !
      call mpas_dmpar_get_index_range(domain % dminfo, 1, nCells, readCellStart, readCellEnd)   
      nReadCells = readCellEnd - readCellStart + 1
   
      call mpas_dmpar_get_index_range(domain % dminfo, 1, nEdges, readEdgeStart, readEdgeEnd)   
      nReadEdges = readEdgeEnd - readEdgeStart + 1
   
      call mpas_dmpar_get_index_range(domain % dminfo, 1, nVertices, readVertexStart, readVertexEnd)   
      nReadVertices = readVertexEnd - readVertexStart + 1

      allocate(readingBlock)
      readingBlock % domain => domain
      readingBlock % blockID = domain % dminfo % my_proc_id
      readingBlock % localBlockID = 0

      !
      ! Allocate and read fields that we will need in order to ultimately work out
      !   which cells/edges/vertices are owned by each block, and which are ghost
      !

      call mpas_io_setup_cell_block_fields(inputHandle, nreadCells, readCellStart, readingBlock, maxEdges, indexTocellIDField, xCellField, &
                                           yCellField, zCellField, nEdgesOnCellField, cellsOnCellField, edgesOnCellField, verticesOnCellField)

      call mpas_io_setup_edge_block_fields(inputHandle, nReadEdges, readEdgeStart, readingBlock, indexToEdgeIDField, xEdgeField, yEdgeField, zEdgeField, cellsOnEdgeField)

      call mpas_io_setup_vertex_block_fields(inputHandle, nReadVertices, readVertexStart, readingBlock, vertexDegree, indexToVertexIDField, &
                                             xVertexField, yVertexField, zVertexField, cellsOnVertexField)
      !
      ! Set up a graph derived data type describing the connectivity for the cells 
      !   that were read by this process
      ! A partial description is passed to the block decomp module by each process,
      !   and the block decomp module returns with a list of global cell indices
      !   that belong to the block on this process
      !
      partial_global_graph_info % nVertices = nReadCells
      partial_global_graph_info % nVerticesTotal = nCells
      partial_global_graph_info % maxDegree = maxEdges
      partial_global_graph_info % ghostStart = nVertices+1
      allocate(partial_global_graph_info % vertexID(nReadCells))
      allocate(partial_global_graph_info % nAdjacent(nReadCells))
      allocate(partial_global_graph_info % adjacencyList(maxEdges, nReadCells))
   
      partial_global_graph_info % vertexID(:) = indexToCellIDField % array(:)
      partial_global_graph_info % nAdjacent(:) = nEdgesOnCellField % array(:)
      partial_global_graph_info % adjacencyList(:,:) = cellsOnCellField % array(:,:)
   
      ! TODO: Ensure (by renaming or exchanging) that initial cell range on each proc is contiguous
      !       This situation may occur when reading a restart file with cells/edges/vertices written
      !       in a scrambled order
   

      ! Determine which cells are owned by this process
      call mpas_block_decomp_cells_for_proc(domain % dminfo, partial_global_graph_info, local_cell_list, block_id, block_start, block_count)

      deallocate(partial_global_graph_info % vertexID)
      deallocate(partial_global_graph_info % nAdjacent)
      deallocate(partial_global_graph_info % adjacencyList)

      call mpas_block_creator_setup_blocks_and_0halo_cells(domain, indexToCellID_Block, local_cell_list, block_id, block_start, block_count)
      call mpas_block_creator_build_0halo_cell_fields(indexToCellIDField, nEdgesOnCellField, cellsOnCellField, verticesOnCellField, edgesOnCellField, indexToCellID_Block, nEdgesOnCell_Block, cellsOnCell_Block, verticesOnCell_Block, edgesOnCell_Block)

      call mpas_block_creator_build_0_and_1halo_edge_fields(indexToEdgeIDField, cellsOnEdgeField, indexToCellID_Block, nEdgesOnCell_Block, edgesOnCell_Block, indexToEdgeID_Block, cellsOnEdge_Block, nEdgesSolveField)
      call mpas_block_creator_build_0_and_1halo_edge_fields(indexToVertexIDField, cellsOnVertexField, indexToCellID_Block, nEdgesOnCell_Block, verticesOnCell_Block, indexToVertexID_Block, cellsOnVertex_Block, nVerticesSolveField)

      call mpas_block_creator_build_cell_halos(indexToCellID_Block, nEdgesOnCell_Block, cellsOnCell_Block, verticesOnCell_Block, edgesOnCell_Block, nCellsSolveField)

      call mpas_block_creator_build_edge_halos(indexToCellID_Block, nEdgesOnCell_Block, nCellsSolveField, edgesOnCell_Block, indexToEdgeID_Block, cellsOnEdge_Block, nEdgesSolveField)
      call mpas_block_creator_build_edge_halos(indexToCellID_Block, nEdgesOnCell_Block, nCellsSolveField, verticesOnCell_Block, indexToVertexID_Block, cellsOnVertex_Block, nVerticesSolveField)


     ! Allocate blocks, and copy indexTo arrays into blocks
     call mpas_block_creator_finalize_block_init(domain % blocklist, &
                            nCells, nEdges, maxEdges, maxEdges2, nVertices, vertexDegree, nVertLevels &
                             , nCellsSolveField, nEdgesSolveField, nVerticesSolveField, indexToCellID_Block, indexToEdgeID_Block, indexToVertexID_Block)


      call mpas_io_input_init(input_obj, domain % blocklist, domain % dminfo)

      call MPAS_readStreamAtt(input_obj % io_stream, 'sphere_radius', r_sphere_radius, ierr)
      if (ierr /= MPAS_STREAM_NOERR) then
        write(stderrUnit,*) 'Warning: Attribute sphere_radius not found in '//trim(input_obj % filename)
        write(stderrUnit,*) '   Setting sphere_radius to 1.0'
        domain % blocklist % mesh % sphere_radius = 1.0
      else
        domain % blocklist % mesh % sphere_radius = r_sphere_radius
      end if

      call MPAS_readStreamAtt(input_obj % io_stream, 'on_a_sphere', c_on_a_sphere, ierr)
      if (ierr /= MPAS_STREAM_NOERR) then
        write(stderrUnit,*) 'Warning: Attribute on_a_sphere not found in '//trim(input_obj % filename)
        write(stderrUnit,*) '   Setting on_a_sphere to ''YES'''
        domain % blocklist % mesh % on_a_sphere = .true.
      else
        if (index(c_on_a_sphere, 'YES') /= 0) then
          domain % blocklist % mesh % on_a_sphere = .true.
        else
          domain % blocklist % mesh % on_a_sphere = .false.
        end if
      end if

      call MPAS_readStreamAtt(input_obj % io_stream, 'history', domain % history, ierr)
      if (ierr /= MPAS_STREAM_NOERR) then
        write(stderrUnit,*) 'Warning: Attribute History not found in '//trim(input_obj % filename)
        write(stderrUnit,*) '   Setting History to '''''
        domain % history = ""
      else
        ! Remove C String NULL characters, replace C String newlines with semicolons
        do i = 1, len(domain % history)
          if(iachar(domain % history(i:i)) == 0) then
            domain % history(i:i) = " "
          else if(iachar(domain % history(i:i)) == 10) then
            domain % history(i:i) = ";"
          end if
        end do
      end if

      block_ptr => domain % blocklist % next
      do while (associated(block_ptr))
        block_ptr % mesh % sphere_radius = domain % blocklist % mesh % sphere_radius
        block_ptr % mesh % on_a_sphere = domain % blocklist % mesh % on_a_sphere

        ! Link the sendList and recvList pointers in each field type to the appropriate lists 
        !   in parinfo, e.g., cellsToSend and cellsToRecv; in future, it can also be extended to 
        !   link blocks of fields to eachother
        call mpas_create_field_links(block_ptr)

        block_ptr => block_ptr % next
      end do

      if (.not. config_do_restart) then
        input_obj % time = 1
      else
        !
        ! If doing a restart, we need to decide which time slice to read from the 
        !   restart file
        !
        input_obj % time = MPAS_seekStream(input_obj % io_stream, restartTimeStamp, MPAS_STREAM_EXACT_TIME, timeStamp, ierr)
        if (ierr == MPAS_IO_ERR) then
          write(stderrUnit,*) 'Error: restart file '//trim(filename)//' did not contain time '//trim(restartTimeStamp)
          call mpas_dmpar_abort(domain % dminfo)
        end if

!       input_obj % time = MPAS_seekStream(input_obj % io_stream, config_start_time, MPAS_STREAM_EXACT_TIME, timeStamp, ierr)
!       if (ierr == MPAS_IO_ERR) then
!         write(stderrUnit,*) 'Error: restart file '//trim(filename)//' did not contain time '//trim(config_start_time)
!         call mpas_dmpar_abort(domain % dminfo)
!       end if
!write(stderrUnit,*) 'MGD DEBUGGING time = ', input_obj % time
        write(stderrUnit,*) 'Restarting model from time ', trim(timeStamp)
      end if

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      ! Do the actual work of reading all fields in from the input or restart file
      ! For each field:
      !   1) Each process reads a contiguous range of cell/edge/vertex indices, which
      !      may not correspond with the cells/edges/vertices that are owned by the
      !      process
      !   2) All processes then send the global indices that were read to the 
      !      processes that own those indices based on 
      !      {send,recv}{Cell,Edge,Vertex}List
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      call mpas_read_and_distribute_fields(input_obj)

      call mpas_io_input_finalize(input_obj, domain % dminfo)

      call MPAS_io_close(inputHandle, ierr)

      !
      ! Exchange halos for all of the fields that were read from the input file
      !
      call mpas_exch_input_field_halos(domain, input_obj)

      call mpas_block_creator_reindex_block_fields(domain % blocklist)

      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToCellIDField % sendList)
      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToCellIDField % recvList)
      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToCellIDField % copyList)

      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToEdgeIDField % sendList)
      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToEdgeIDField % recvList)
      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToEdgeIDField % copyList)

      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToVertexIDField % sendList)
      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToVertexIDField % recvList)
      call mpas_dmpar_destroy_mulithalo_exchange_list(indexToVertexIDField % copyList)

      call mpas_deallocate_field(indexToCellIDField)
      call mpas_deallocate_field(indexToEdgeIDField)
      call mpas_deallocate_field(indexToVertexIDField)
      call mpas_deallocate_field(cellsOnCellField)

      call mpas_deallocate_field(edgesOnCellField)
      call mpas_deallocate_field(verticesOnCellField)
      call mpas_deallocate_field(cellsOnEdgeField)
      call mpas_deallocate_field(cellsOnVertexField)

      call mpas_deallocate_field(indexToCellID_Block)
      call mpas_deallocate_field(nEdgesOnCell_Block)
      call mpas_deallocate_field(cellsOnCell_Block)
      call mpas_deallocate_field(verticesOnCell_Block)
      call mpas_deallocate_field(edgesOnCell_Block)
      call mpas_deallocate_field(indexToVertexID_Block)
      call mpas_deallocate_field(cellsOnVertex_Block)
      call mpas_deallocate_field(indexToEdgeID_Block)
      call mpas_deallocate_field(cellsOnEdge_Block)

      call mpas_deallocate_field(nCellsSolveField)
      call mpas_deallocate_field(nVerticesSolveField)
      call mpas_deallocate_field(nEdgesSolveField)

      deallocate(local_cell_list)
      deallocate(block_id)
      deallocate(block_start)
      deallocate(block_count)
      deallocate(readingBlock)

   end subroutine mpas_input_state_for_domain!}}}

   !CR:TODO: an identical subroutine is found in module_io_output - merge
   subroutine mpas_insert_string_suffix(stream, suffix, filename)!{{{

      implicit none

      character (len=*), intent(in) :: stream
      character (len=*), intent(in) :: suffix
      character (len=*), intent(out) :: filename
      integer :: length, i

      filename = trim(stream) // '.' // trim(suffix)

      length = len_trim(stream)
      do i=length-1,1,-1
         if(stream(i:i) == '.') then
            filename = trim(stream(:i)) // trim(suffix) // trim(stream(i:))
            exit
         end if
      end do

      do i=1,len_trim(filename)
         if (filename(i:i) == ':') filename(i:i) = '.'
      end do

   end subroutine mpas_insert_string_suffix!}}}

   subroutine mpas_read_and_distribute_fields(input_obj)!{{{
      
      implicit none

      type (io_input_object), intent(inout) :: input_obj

      integer :: ierr


      call MPAS_readStream(input_obj % io_stream, input_obj % time, ierr)


   end subroutine mpas_read_and_distribute_fields!}}}

   subroutine mpas_io_input_init(input_obj, blocklist, dminfo)!{{{
 
      implicit none

      type (io_input_object), intent(inout) :: input_obj
      type (block_type), intent(in) :: blocklist
      type (dm_info), intent(in) :: dminfo
 
      integer :: nferr
 
      call MPAS_createStream(input_obj % io_stream, trim(input_obj % filename), MPAS_IO_PNETCDF, MPAS_IO_READ, 1, nferr)
      if (nferr /= MPAS_STREAM_NOERR) then
         write(stderrUnit,*) ' '
         if (input_obj % stream == STREAM_RESTART) then
            write(stderrUnit,*) 'Error opening restart file ''', trim(input_obj % filename), ''''
         else if (input_obj % stream == STREAM_INPUT) then
            write(stderrUnit,*) 'Error opening input file ''', trim(input_obj % filename), ''''
         else if (input_obj % stream == STREAM_SFC) then
            write(stderrUnit,*) 'Error opening sfc file ''', trim(input_obj % filename), ''''
         end if
         write(stderrUnit,*) ' '
         call mpas_dmpar_abort(dminfo)
      end if

      if ((blocklist % state % time_levs(1) % state % tracers % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % state % time_levs(1) % state % tracers % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % tracers % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % tracers, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % normalVelocity, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % layerThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % state % time_levs(1) % state % layerThickness % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % layerThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % layerThickness, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % ssh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % state % time_levs(1) % state % ssh % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % ssh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % ssh, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % highFreqThickness, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % lowFreqDivergence, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
          (blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % normalBarotropicVelocity, &
nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % input .and. input_obj % stream &
== STREAM_INPUT) .or. &
          (blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % restart .and. input_obj % stream &
== STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % &
normalBarotropicVelocitySubcycle, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
          (blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % sshSubcycle, nferr)
      end if

      if ((blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
          (blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % state % time_levs(1) % state % normalBaroclinicVelocity, &
nferr)
      end if

      if ((blocklist % mesh % latCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % latCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % latCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % latCell, nferr)
      end if

      if ((blocklist % mesh % lonCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % lonCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % lonCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % lonCell, nferr)
      end if

      if ((blocklist % mesh % xCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % xCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % xCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % xCell, nferr)
      end if

      if ((blocklist % mesh % yCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % yCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % yCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % yCell, nferr)
      end if

      if ((blocklist % mesh % zCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % zCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % zCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % zCell, nferr)
      end if

      if ((blocklist % mesh % indexToCellID % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % indexToCellID % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % indexToCellID % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % indexToCellID, nferr)
      end if

      if ((blocklist % mesh % latEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % latEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % latEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % latEdge, nferr)
      end if

      if ((blocklist % mesh % lonEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % lonEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % lonEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % lonEdge, nferr)
      end if

      if ((blocklist % mesh % xEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % xEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % xEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % xEdge, nferr)
      end if

      if ((blocklist % mesh % yEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % yEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % yEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % yEdge, nferr)
      end if

      if ((blocklist % mesh % zEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % zEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % zEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % zEdge, nferr)
      end if

      if ((blocklist % mesh % indexToEdgeID % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % indexToEdgeID % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % indexToEdgeID % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % indexToEdgeID, nferr)
      end if

      if ((blocklist % mesh % latVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % latVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % latVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % latVertex, nferr)
      end if

      if ((blocklist % mesh % lonVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % lonVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % lonVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % lonVertex, nferr)
      end if

      if ((blocklist % mesh % xVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % xVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % xVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % xVertex, nferr)
      end if

      if ((blocklist % mesh % yVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % yVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % yVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % yVertex, nferr)
      end if

      if ((blocklist % mesh % zVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % zVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % zVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % zVertex, nferr)
      end if

      if ((blocklist % mesh % indexToVertexID % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % indexToVertexID % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % indexToVertexID % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % indexToVertexID, nferr)
      end if

      if ((blocklist % mesh % meshDensity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % meshDensity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % meshDensity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % meshDensity, nferr)
      end if

      if ((blocklist % mesh % meshScalingDel2 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % meshScalingDel2 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % meshScalingDel2 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % meshScalingDel2, nferr)
      end if

      if ((blocklist % mesh % meshScalingDel4 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % meshScalingDel4 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % meshScalingDel4 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % meshScalingDel4, nferr)
      end if

      if ((blocklist % mesh % meshScaling % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % meshScaling % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % meshScaling % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % meshScaling, nferr)
      end if

      if ((blocklist % mesh % cellsOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % cellsOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % cellsOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % cellsOnEdge, nferr)
      end if

      if ((blocklist % mesh % nEdgesOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % nEdgesOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % nEdgesOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % nEdgesOnCell, nferr)
      end if

      if ((blocklist % mesh % nEdgesOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % nEdgesOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % nEdgesOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % nEdgesOnEdge, nferr)
      end if

      if ((blocklist % mesh % edgesOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgesOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgesOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgesOnCell, nferr)
      end if

      if ((blocklist % mesh % edgesOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgesOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgesOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgesOnEdge, nferr)
      end if

      if ((blocklist % mesh % weightsOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % weightsOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % weightsOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % weightsOnEdge, nferr)
      end if

      if ((blocklist % mesh % dvEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % dvEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % dvEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % dvEdge, nferr)
      end if

      if ((blocklist % mesh % dcEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % dcEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % dcEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % dcEdge, nferr)
      end if

      if ((blocklist % mesh % angleEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % angleEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % angleEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % angleEdge, nferr)
      end if

      if ((blocklist % mesh % areaCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % areaCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % areaCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % areaCell, nferr)
      end if

      if ((blocklist % mesh % areaTriangle % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % areaTriangle % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % areaTriangle % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % areaTriangle, nferr)
      end if

      if ((blocklist % mesh % edgeNormalVectors % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgeNormalVectors % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgeNormalVectors % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgeNormalVectors, nferr)
      end if

      if ((blocklist % mesh % edgeTangentVectors % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgeTangentVectors % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgeTangentVectors % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgeTangentVectors, nferr)
      end if

      if ((blocklist % mesh % localVerticalUnitVectors % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % localVerticalUnitVectors % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % localVerticalUnitVectors % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % localVerticalUnitVectors, nferr)
      end if

      if ((blocklist % mesh % cellTangentPlane % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % cellTangentPlane % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % cellTangentPlane % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % cellTangentPlane, nferr)
      end if

      if ((blocklist % mesh % cellsOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % cellsOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % cellsOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % cellsOnCell, nferr)
      end if

      if ((blocklist % mesh % verticesOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % verticesOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % verticesOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % verticesOnCell, nferr)
      end if

      if ((blocklist % mesh % verticesOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % verticesOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % verticesOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % verticesOnEdge, nferr)
      end if

      if ((blocklist % mesh % edgesOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgesOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgesOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgesOnVertex, nferr)
      end if

      if ((blocklist % mesh % cellsOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % cellsOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % cellsOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % cellsOnVertex, nferr)
      end if

      if ((blocklist % mesh % kiteAreasOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % kiteAreasOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % kiteAreasOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % kiteAreasOnVertex, nferr)
      end if

      if ((blocklist % mesh % fEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % fEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % fEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % fEdge, nferr)
      end if

      if ((blocklist % mesh % fVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % fVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % fVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % fVertex, nferr)
      end if

      if ((blocklist % mesh % fCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % fCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % fCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % fCell, nferr)
      end if

      if ((blocklist % mesh % bottomDepth % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % bottomDepth % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % bottomDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % bottomDepth, nferr)
      end if

      if ((blocklist % mesh % derivTwo % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % derivTwo % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % derivTwo % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % derivTwo, nferr)
      end if

      if ((blocklist % mesh % advCoefs % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % advCoefs % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % advCoefs % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % advCoefs, nferr)
      end if

      if ((blocklist % mesh % advCoefs3rd % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % advCoefs3rd % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % advCoefs3rd % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % advCoefs3rd, nferr)
      end if

      if ((blocklist % mesh % advCellsForEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % advCellsForEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % advCellsForEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % advCellsForEdge, nferr)
      end if

      if ((blocklist % mesh % nAdvCellsForEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % nAdvCellsForEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % nAdvCellsForEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % nAdvCellsForEdge, nferr)
      end if

      if ((blocklist % mesh % highOrderAdvectionMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % highOrderAdvectionMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % highOrderAdvectionMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % highOrderAdvectionMask, nferr)
      end if

      if ((blocklist % mesh % coeffs_reconstruct % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % coeffs_reconstruct % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % coeffs_reconstruct % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % coeffs_reconstruct, nferr)
      end if

      if ((blocklist % mesh % maxLevelCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % maxLevelCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % maxLevelCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % maxLevelCell, nferr)
      end if

      if ((blocklist % mesh % maxLevelEdgeTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % maxLevelEdgeTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % maxLevelEdgeTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % maxLevelEdgeTop, nferr)
      end if

      if ((blocklist % mesh % maxLevelEdgeBot % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % maxLevelEdgeBot % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % maxLevelEdgeBot % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % maxLevelEdgeBot, nferr)
      end if

      if ((blocklist % mesh % maxLevelVertexTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % maxLevelVertexTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % maxLevelVertexTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % maxLevelVertexTop, nferr)
      end if

      if ((blocklist % mesh % maxLevelVertexBot % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % maxLevelVertexBot % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % maxLevelVertexBot % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % maxLevelVertexBot, nferr)
      end if

      if ((blocklist % mesh % refBottomDepth % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % refBottomDepth % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % refBottomDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % refBottomDepth, nferr)
      end if

      if ((blocklist % mesh % refBottomDepthTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % refBottomDepthTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % refBottomDepthTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % refBottomDepthTopOfCell, nferr)
      end if

      if ((blocklist % mesh % vertCoordMovementWeights % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % vertCoordMovementWeights % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % vertCoordMovementWeights % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % vertCoordMovementWeights, nferr)
      end if

      if ((blocklist % mesh % boundaryEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % boundaryEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % boundaryEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % boundaryEdge, nferr)
      end if

      if ((blocklist % mesh % boundaryVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % boundaryVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % boundaryVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % boundaryVertex, nferr)
      end if

      if ((blocklist % mesh % boundaryCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % boundaryCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % boundaryCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % boundaryCell, nferr)
      end if

      if ((blocklist % mesh % edgeMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgeMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgeMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgeMask, nferr)
      end if

      if ((blocklist % mesh % vertexMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % vertexMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % vertexMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % vertexMask, nferr)
      end if

      if ((blocklist % mesh % cellMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % cellMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % cellMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % cellMask, nferr)
      end if

      if ((blocklist % mesh % temperatureRestore % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % temperatureRestore % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % temperatureRestore % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % temperatureRestore, nferr)
      end if

      if ((blocklist % mesh % salinityRestore % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % salinityRestore % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % salinityRestore % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % salinityRestore, nferr)
      end if

      if ((blocklist % mesh % edgeSignOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgeSignOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgeSignOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgeSignOnCell, nferr)
      end if

      if ((blocklist % mesh % edgeSignOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % edgeSignOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % edgeSignOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % edgeSignOnVertex, nferr)
      end if

      if ((blocklist % mesh % kiteIndexOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % mesh % kiteIndexOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % mesh % kiteIndexOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % mesh % kiteIndexOnCell, nferr)
      end if

      if ((blocklist % verticalMesh % restingThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % verticalMesh % restingThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % verticalMesh % restingThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % verticalMesh % restingThickness, nferr)
      end if

      if ((blocklist % verticalMesh % refZMid % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % verticalMesh % refZMid % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % verticalMesh % refZMid % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % verticalMesh % refZMid, nferr)
      end if

      if ((blocklist % verticalMesh % refLayerThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % verticalMesh % refLayerThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % verticalMesh % refLayerThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % verticalMesh % refLayerThickness, nferr)
      end if

      if ((blocklist % tend % tracers % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % tend % tracers % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % tend % tracers % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % tend % tracers, nferr)
      end if

      if ((blocklist % tend % normalVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % tend % normalVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % tend % normalVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % tend % normalVelocity, nferr)
      end if

      if ((blocklist % tend % layerThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % tend % layerThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % tend % layerThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % tend % layerThickness, nferr)
      end if

      if ((blocklist % tend % ssh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % tend % ssh % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % tend % ssh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % tend % ssh, nferr)
      end if

      if ((blocklist % tend % highFreqThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % tend % highFreqThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % tend % highFreqThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % tend % highFreqThickness, nferr)
      end if

      if ((blocklist % tend % lowFreqDivergence % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % tend % lowFreqDivergence % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % tend % lowFreqDivergence % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % tend % lowFreqDivergence, nferr)
      end if

      if ((blocklist % diagnostics % tracersSurfaceValue % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % tracersSurfaceValue % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % tracersSurfaceValue % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % tracersSurfaceValue, nferr)
      end if

      if ((blocklist % diagnostics % surfaceVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % surfaceVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % surfaceVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % surfaceVelocity, nferr)
      end if

      if ((blocklist % diagnostics % SSHGradient % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % SSHGradient % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % SSHGradient % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % SSHGradient, nferr)
      end if

      if ((blocklist % diagnostics % xtime % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % xtime % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % xtime % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % xtime, nferr)
      end if

      if ((blocklist % diagnostics % zMid % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % zMid % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % zMid % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % zMid, nferr)
      end if

      if ((blocklist % diagnostics % zTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % zTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % zTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % zTop, nferr)
      end if

      if ((blocklist % diagnostics % density % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % density % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % density % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % density, nferr)
      end if

      if ((blocklist % diagnostics % displacedDensity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % displacedDensity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % displacedDensity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % displacedDensity, nferr)
      end if

      if ((blocklist % diagnostics % potentialDensity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % potentialDensity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % potentialDensity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % potentialDensity, nferr)
      end if

      if ((blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % BruntVaisalaFreqTop, nferr)
      end if

      if ((blocklist % diagnostics % montgomeryPotential % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % montgomeryPotential % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % montgomeryPotential % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % montgomeryPotential, nferr)
      end if

      if ((blocklist % diagnostics % pressure % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % pressure % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % pressure % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % pressure, nferr)
      end if

      if ((blocklist % diagnostics % uTransport % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % uTransport % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % uTransport % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % uTransport, nferr)
      end if

      if ((blocklist % diagnostics % vertTransportVelocityTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % vertTransportVelocityTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % vertTransportVelocityTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % vertTransportVelocityTop, nferr)
      end if

      if ((blocklist % diagnostics % vertVelocityTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % vertVelocityTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % vertVelocityTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % vertVelocityTop, nferr)
      end if

      if ((blocklist % diagnostics % tangentialVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % tangentialVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % tangentialVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % tangentialVelocity, nferr)
      end if

      if ((blocklist % diagnostics % layerThicknessEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % layerThicknessEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % layerThicknessEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % layerThicknessEdge, nferr)
      end if

      if ((blocklist % diagnostics % layerThicknessVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % layerThicknessVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % layerThicknessVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % layerThicknessVertex, nferr)
      end if

      if ((blocklist % diagnostics % kineticEnergyCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % kineticEnergyCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % kineticEnergyCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % kineticEnergyCell, nferr)
      end if

      if ((blocklist % diagnostics % hEddyFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % hEddyFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % hEddyFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % hEddyFlux, nferr)
      end if

      if ((blocklist % diagnostics % hKappa % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % hKappa % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % hKappa % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % hKappa, nferr)
      end if

      if ((blocklist % diagnostics % hKappaQ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % hKappaQ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % hKappaQ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % hKappaQ, nferr)
      end if

      if ((blocklist % diagnostics % viscosity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % viscosity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % viscosity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % viscosity, nferr)
      end if

      if ((blocklist % diagnostics % divergence % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % divergence % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % divergence % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % divergence, nferr)
      end if

      if ((blocklist % diagnostics % circulation % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % circulation % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % circulation % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % circulation, nferr)
      end if

      if ((blocklist % diagnostics % relativeVorticity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % relativeVorticity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % relativeVorticity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % relativeVorticity, nferr)
      end if

      if ((blocklist % diagnostics % relativeVorticityCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % relativeVorticityCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % relativeVorticityCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % relativeVorticityCell, nferr)
      end if

      if ((blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalizedRelativeVorticityEdge, nferr)
      end if

      if ((blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalizedPlanetaryVorticityEdge, nferr)
      end if

      if ((blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalizedRelativeVorticityCell, nferr)
      end if

      if ((blocklist % diagnostics % barotropicForcing % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % barotropicForcing % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % barotropicForcing % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % barotropicForcing, nferr)
      end if

      if ((blocklist % diagnostics % barotropicThicknessFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % barotropicThicknessFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % barotropicThicknessFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % barotropicThicknessFlux, nferr)
      end if

      if ((blocklist % diagnostics % normalVelocityX % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % normalVelocityX % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalVelocityX % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalVelocityX, nferr)
      end if

      if ((blocklist % diagnostics % normalVelocityY % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % normalVelocityY % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalVelocityY % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalVelocityY, nferr)
      end if

      if ((blocklist % diagnostics % normalVelocityZ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % normalVelocityZ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalVelocityZ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalVelocityZ, nferr)
      end if

      if ((blocklist % diagnostics % normalVelocityZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % normalVelocityZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalVelocityZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalVelocityZonal, nferr)
      end if

      if ((blocklist % diagnostics % normalVelocityMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % normalVelocityMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % normalVelocityMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % normalVelocityMeridional, nferr)
      end if

      if ((blocklist % diagnostics % gradSSH % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % gradSSH % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % gradSSH % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % gradSSH, nferr)
      end if

      if ((blocklist % diagnostics % gradSSHX % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % gradSSHX % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % gradSSHX % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % gradSSHX, nferr)
      end if

      if ((blocklist % diagnostics % gradSSHY % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % gradSSHY % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % gradSSHY % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % gradSSHY, nferr)
      end if

      if ((blocklist % diagnostics % gradSSHZ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % gradSSHZ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % gradSSHZ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % gradSSHZ, nferr)
      end if

      if ((blocklist % diagnostics % gradSSHZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % gradSSHZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % gradSSHZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % gradSSHZonal, nferr)
      end if

      if ((blocklist % diagnostics % gradSSHMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % gradSSHMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % gradSSHMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % gradSSHMeridional, nferr)
      end if

      if ((blocklist % diagnostics % uBolusGM % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % uBolusGM % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % uBolusGM % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % uBolusGM, nferr)
      end if

      if ((blocklist % diagnostics % uBolusGMX % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % uBolusGMX % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % uBolusGMX % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % uBolusGMX, nferr)
      end if

      if ((blocklist % diagnostics % uBolusGMY % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % uBolusGMY % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % uBolusGMY % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % uBolusGMY, nferr)
      end if

      if ((blocklist % diagnostics % uBolusGMZ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % uBolusGMZ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % uBolusGMZ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % uBolusGMZ, nferr)
      end if

      if ((blocklist % diagnostics % uBolusGMZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % uBolusGMZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % uBolusGMZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % uBolusGMZonal, nferr)
      end if

      if ((blocklist % diagnostics % uBolusGMMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % uBolusGMMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % uBolusGMMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % uBolusGMMeridional, nferr)
      end if

      if ((blocklist % diagnostics % RiTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % RiTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % RiTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % RiTopOfCell, nferr)
      end if

      if ((blocklist % diagnostics % RiTopOfEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % RiTopOfEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % RiTopOfEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % RiTopOfEdge, nferr)
      end if

      if ((blocklist % diagnostics % vertViscTopOfEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % vertViscTopOfEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % vertViscTopOfEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % vertViscTopOfEdge, nferr)
      end if

      if ((blocklist % diagnostics % vertViscTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % vertViscTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % vertViscTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % vertViscTopOfCell, nferr)
      end if

      if ((blocklist % diagnostics % vertDiffTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % vertDiffTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % vertDiffTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % vertDiffTopOfCell, nferr)
      end if

      if ((blocklist % diagnostics % bulkRichardsonNumber % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % bulkRichardsonNumber % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % bulkRichardsonNumber % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % bulkRichardsonNumber, nferr)
      end if

      if ((blocklist % diagnostics % boundaryLayerDepth % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % boundaryLayerDepth % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % boundaryLayerDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % boundaryLayerDepth, nferr)
      end if

      if ((blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % indexBoundaryLayerDepth, nferr)
      end if

      if ((blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % surfaceFrictionVelocity, nferr)
      end if

      if ((blocklist % diagnostics % windStressZonalDiag % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % windStressZonalDiag % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % windStressZonalDiag % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % windStressZonalDiag, nferr)
      end if

      if ((blocklist % diagnostics % windStressMeridionalDiag % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % windStressMeridionalDiag % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % windStressMeridionalDiag % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % windStressMeridionalDiag, nferr)
      end if

      if ((blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
          (blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % penetrativeTemperatureFluxOBL, nferr)
      end if

      if ((blocklist % diagnostics % buoyancyForcingOBL % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % buoyancyForcingOBL % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % buoyancyForcingOBL % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % buoyancyForcingOBL, nferr)
      end if

      if ((blocklist % diagnostics % areaCellGlobal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % areaCellGlobal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % areaCellGlobal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % areaCellGlobal, nferr)
      end if

      if ((blocklist % diagnostics % areaEdgeGlobal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % areaEdgeGlobal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % areaEdgeGlobal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % areaEdgeGlobal, nferr)
      end if

      if ((blocklist % diagnostics % areaTriangleGlobal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % areaTriangleGlobal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % areaTriangleGlobal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % areaTriangleGlobal, nferr)
      end if

      if ((blocklist % diagnostics % volumeCellGlobal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % volumeCellGlobal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % volumeCellGlobal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % volumeCellGlobal, nferr)
      end if

      if ((blocklist % diagnostics % volumeEdgeGlobal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % volumeEdgeGlobal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % volumeEdgeGlobal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % volumeEdgeGlobal, nferr)
      end if

      if ((blocklist % diagnostics % CFLNumberGlobal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % diagnostics % CFLNumberGlobal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % diagnostics % CFLNumberGlobal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % diagnostics % CFLNumberGlobal, nferr)
      end if

      if ((blocklist % average % nAverage % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % nAverage % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % nAverage % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % nAverage, nferr)
      end if

      if ((blocklist % average % avgSsh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % avgSsh % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % avgSsh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % avgSsh, nferr)
      end if

      if ((blocklist % average % varSsh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % varSsh % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % varSsh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % varSsh, nferr)
      end if

      if ((blocklist % average % avgVelocityZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % avgVelocityZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % avgVelocityZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % avgVelocityZonal, nferr)
      end if

      if ((blocklist % average % avgVelocityMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % avgVelocityMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % avgVelocityMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % avgVelocityMeridional, nferr)
      end if

      if ((blocklist % average % varVelocityZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % varVelocityZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % varVelocityZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % varVelocityZonal, nferr)
      end if

      if ((blocklist % average % varVelocityMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % varVelocityMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % varVelocityMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % varVelocityMeridional, nferr)
      end if

      if ((blocklist % average % avgNormalVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % avgNormalVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % avgNormalVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % avgNormalVelocity, nferr)
      end if

      if ((blocklist % average % varNormalVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % varNormalVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % varNormalVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % varNormalVelocity, nferr)
      end if

      if ((blocklist % average % avgVertVelocityTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % average % avgVertVelocityTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % average % avgVertVelocityTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % average % avgVertVelocityTop, nferr)
      end if

      if ((blocklist % forcing % surfaceTracerFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % surfaceTracerFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % surfaceTracerFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % surfaceTracerFlux, nferr)
      end if

      if ((blocklist % forcing % avgTracersSurfaceValue % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % avgTracersSurfaceValue % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % avgTracersSurfaceValue % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % avgTracersSurfaceValue, nferr)
      end if

      if ((blocklist % forcing % avgSurfaceVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % avgSurfaceVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % avgSurfaceVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % avgSurfaceVelocity, nferr)
      end if

      if ((blocklist % forcing % avgSSHGradient % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % avgSSHGradient % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % avgSSHGradient % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % avgSSHGradient, nferr)
      end if

      if ((blocklist % forcing % surfaceWindStress % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % surfaceWindStress % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % surfaceWindStress % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % surfaceWindStress, nferr)
      end if

      if ((blocklist % forcing % surfaceWindStressMagnitude % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % surfaceWindStressMagnitude % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % surfaceWindStressMagnitude % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % surfaceWindStressMagnitude, nferr)
      end if

      if ((blocklist % forcing % surfaceMassFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % surfaceMassFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % surfaceMassFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % surfaceMassFlux, nferr)
      end if

      if ((blocklist % forcing % seaSurfacePressure % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % seaSurfacePressure % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % seaSurfacePressure % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % seaSurfacePressure, nferr)
      end if

      if ((blocklist % forcing % seaIceEnergy % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % seaIceEnergy % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % seaIceEnergy % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % seaIceEnergy, nferr)
      end if

      if ((blocklist % forcing % penetrativeTemperatureFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % penetrativeTemperatureFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % penetrativeTemperatureFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % penetrativeTemperatureFlux, nferr)
      end if

      if ((blocklist % forcing % transmissionCoefficients % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % transmissionCoefficients % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % transmissionCoefficients % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % transmissionCoefficients, nferr)
      end if

      if ((blocklist % forcing % windStressZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % windStressZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % windStressZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % windStressZonal, nferr)
      end if

      if ((blocklist % forcing % windStressMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % windStressMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % windStressMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % windStressMeridional, nferr)
      end if

      if ((blocklist % forcing % latentHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % latentHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % latentHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % latentHeatFlux, nferr)
      end if

      if ((blocklist % forcing % sensibleHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % sensibleHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % sensibleHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % sensibleHeatFlux, nferr)
      end if

      if ((blocklist % forcing % longWaveHeatFluxUp % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % longWaveHeatFluxUp % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % longWaveHeatFluxUp % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % longWaveHeatFluxUp, nferr)
      end if

      if ((blocklist % forcing % longWaveHeatFluxDown % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % longWaveHeatFluxDown % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % longWaveHeatFluxDown % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % longWaveHeatFluxDown, nferr)
      end if

      if ((blocklist % forcing % seaIceHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % seaIceHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % seaIceHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % seaIceHeatFlux, nferr)
      end if

      if ((blocklist % forcing % shortWaveHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % shortWaveHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % shortWaveHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % shortWaveHeatFlux, nferr)
      end if

      if ((blocklist % forcing % evaporationFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % evaporationFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % evaporationFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % evaporationFlux, nferr)
      end if

      if ((blocklist % forcing % seaIceSalinityFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % seaIceSalinityFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % seaIceSalinityFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % seaIceSalinityFlux, nferr)
      end if

      if ((blocklist % forcing % seaIceFreshWaterFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % seaIceFreshWaterFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % seaIceFreshWaterFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % seaIceFreshWaterFlux, nferr)
      end if

      if ((blocklist % forcing % riverRunoffFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % riverRunoffFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % riverRunoffFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % riverRunoffFlux, nferr)
      end if

      if ((blocklist % forcing % iceRunoffFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % iceRunoffFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % iceRunoffFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % iceRunoffFlux, nferr)
      end if

      if ((blocklist % forcing % rainFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % rainFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % rainFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % rainFlux, nferr)
      end if

      if ((blocklist % forcing % snowFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % snowFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % snowFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % snowFlux, nferr)
      end if

      if ((blocklist % forcing % iceFraction % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % iceFraction % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % iceFraction % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % iceFraction, nferr)
      end if

      if ((blocklist % forcing % prognosticCO2 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % prognosticCO2 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % prognosticCO2 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % prognosticCO2, nferr)
      end if

      if ((blocklist % forcing % diagnosticCO2 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % diagnosticCO2 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % diagnosticCO2 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % diagnosticCO2, nferr)
      end if

      if ((blocklist % forcing % squaredWindSpeed10Meter % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % squaredWindSpeed10Meter % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % squaredWindSpeed10Meter % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % squaredWindSpeed10Meter, nferr)
      end if

      if ((blocklist % forcing % nAccumulatedCoupled % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % nAccumulatedCoupled % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % nAccumulatedCoupled % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % nAccumulatedCoupled, nferr)
      end if

      if ((blocklist % forcing % CO2Flux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % CO2Flux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % CO2Flux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % CO2Flux, nferr)
      end if

      if ((blocklist % forcing % DMSFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % forcing % DMSFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % forcing % DMSFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % forcing % DMSFlux, nferr)
      end if

      if ((blocklist % scratch % vorticityGradientTangentialComponent % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % scratch % vorticityGradientTangentialComponent % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
          (blocklist % scratch % vorticityGradientTangentialComponent % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % vorticityGradientTangentialComponent, nferr)
      end if

      if ((blocklist % scratch % vorticityGradientNormalComponent % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % vorticityGradientNormalComponent % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
          (blocklist % scratch % vorticityGradientNormalComponent % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % vorticityGradientNormalComponent, nferr)
      end if

      if ((blocklist % scratch % normalizedRelativeVorticityVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % scratch % normalizedRelativeVorticityVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
          (blocklist % scratch % normalizedRelativeVorticityVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % normalizedRelativeVorticityVertex, nferr)
      end if

      if ((blocklist % scratch % normalizedPlanetaryVorticityVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
          (blocklist % scratch % normalizedPlanetaryVorticityVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
          (blocklist % scratch % normalizedPlanetaryVorticityVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % normalizedPlanetaryVorticityVertex, nferr)
      end if

      if ((blocklist % scratch % kineticEnergyVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % kineticEnergyVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % kineticEnergyVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % kineticEnergyVertex, nferr)
      end if

      if ((blocklist % scratch % kineticEnergyVertexOnCells % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % kineticEnergyVertexOnCells % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % kineticEnergyVertexOnCells % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % kineticEnergyVertexOnCells, nferr)
      end if

      if ((blocklist % scratch % densitySurfaceDisplaced % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % densitySurfaceDisplaced % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % densitySurfaceDisplaced % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % densitySurfaceDisplaced, nferr)
      end if

      if ((blocklist % scratch % thermalExpansionCoeff % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % thermalExpansionCoeff % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % thermalExpansionCoeff % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % thermalExpansionCoeff, nferr)
      end if

      if ((blocklist % scratch % salineContractionCoeff % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % salineContractionCoeff % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % salineContractionCoeff % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % salineContractionCoeff, nferr)
      end if

      if ((blocklist % scratch % normalVelocityTest % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % normalVelocityTest % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % normalVelocityTest % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % normalVelocityTest, nferr)
      end if

      if ((blocklist % scratch % tangentialVelocityTest % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % tangentialVelocityTest % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % tangentialVelocityTest % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % tangentialVelocityTest, nferr)
      end if

      if ((blocklist % scratch % strainRateR3Cell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % strainRateR3Cell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % strainRateR3Cell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % strainRateR3Cell, nferr)
      end if

      if ((blocklist % scratch % strainRateR3CellSolution % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % strainRateR3CellSolution % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % strainRateR3CellSolution % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % strainRateR3CellSolution, nferr)
      end if

      if ((blocklist % scratch % strainRateR3Edge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % strainRateR3Edge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % strainRateR3Edge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % strainRateR3Edge, nferr)
      end if

      if ((blocklist % scratch % strainRateLonLatRCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % strainRateLonLatRCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % strainRateLonLatRCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % strainRateLonLatRCell, nferr)
      end if

      if ((blocklist % scratch % strainRateLonLatRCellSolution % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % strainRateLonLatRCellSolution % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
          (blocklist % scratch % strainRateLonLatRCellSolution % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % strainRateLonLatRCellSolution, nferr)
      end if

      if ((blocklist % scratch % strainRateLonLatREdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % strainRateLonLatREdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % strainRateLonLatREdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % strainRateLonLatREdge, nferr)
      end if

      if ((blocklist % scratch % divTensorR3Cell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % divTensorR3Cell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % divTensorR3Cell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % divTensorR3Cell, nferr)
      end if

      if ((blocklist % scratch % divTensorR3CellSolution % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % divTensorR3CellSolution % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % divTensorR3CellSolution % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % divTensorR3CellSolution, nferr)
      end if

      if ((blocklist % scratch % divTensorLonLatRCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % divTensorLonLatRCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % divTensorLonLatRCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % divTensorLonLatRCell, nferr)
      end if

      if ((blocklist % scratch % divTensorLonLatRCellSolution % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % divTensorLonLatRCellSolution % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % divTensorLonLatRCellSolution % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % divTensorLonLatRCellSolution, nferr)
      end if

      if ((blocklist % scratch % outerProductEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % outerProductEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % outerProductEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % outerProductEdge, nferr)
      end if

      if ((blocklist % scratch % normalVectorEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % normalVectorEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % normalVectorEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % normalVectorEdge, nferr)
      end if

      if ((blocklist % scratch % tangentialVectorEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % tangentialVectorEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % tangentialVectorEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % tangentialVectorEdge, nferr)
      end if

      if ((blocklist % scratch % windStressFull % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % windStressFull % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % windStressFull % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % windStressFull, nferr)
      end if

      if ((blocklist % scratch % windStressX % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % windStressX % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % windStressX % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % windStressX, nferr)
      end if

      if ((blocklist % scratch % windStressY % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % windStressY % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % windStressY % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % windStressY, nferr)
      end if

      if ((blocklist % scratch % windStressZ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % windStressZ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % windStressZ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % windStressZ, nferr)
      end if

      if ((blocklist % scratch % windStressZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % windStressZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % windStressZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % windStressZonal, nferr)
      end if

      if ((blocklist % scratch % windStressMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
          (blocklist % scratch % windStressMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
          (blocklist % scratch % windStressMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
         call MPAS_streamAddField(input_obj % io_stream, blocklist % scratch % windStressMeridional, nferr)
      end if


   end subroutine mpas_io_input_init!}}}

   subroutine mpas_exch_input_field_halos(domain, input_obj)!{{{

      implicit none

      type (domain_type), intent(inout) :: domain
      type (io_input_object), intent(inout) :: input_obj

      if (domain % blocklist % state % time_levs(1) % state % tracers % isPersistent .and. domain % blocklist % state % &
time_levs(1) % state % tracers % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % tracers % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % tracers % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % tracers % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % tracers)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % normalVelocity % isPersistent .and. domain % blocklist % state % &
time_levs(1) % state % normalVelocity % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalVelocity % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % normalVelocity)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % layerThickness % isPersistent .and. domain % blocklist % state % &
time_levs(1) % state % layerThickness % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % layerThickness % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % layerThickness % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % layerThickness % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % layerThickness)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % ssh % isPersistent .and. domain % blocklist % state % time_levs(1) % &
state % ssh % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % ssh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % state % time_levs(1) % state % ssh % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % ssh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % ssh)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % highFreqThickness % isPersistent .and. domain % blocklist % state % &
time_levs(1) % state % highFreqThickness % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % highFreqThickness % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % highFreqThickness)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % lowFreqDivergence % isPersistent .and. domain % blocklist % state % &
time_levs(1) % state % lowFreqDivergence % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % lowFreqDivergence % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % lowFreqDivergence)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity % isPersistent .and. domain % blocklist % &
state % time_levs(1) % state % normalBarotropicVelocity % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % input .and. input_obj % &
stream == STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % restart .and. input_obj % &
stream == STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity % ioinfo % sfc .and. input_obj % stream &
== STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocity)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % isPersistent .and. domain % &
blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % input .and. &
input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % restart .and. &
input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle % ioinfo % sfc .and. input_obj &
% stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % normalBarotropicVelocitySubcycle)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % sshSubcycle % isPersistent .and. domain % blocklist % state % &
time_levs(1) % state % sshSubcycle % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % sshSubcycle % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % sshSubcycle)
         end if

      end if

      if (domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % isPersistent .and. domain % blocklist % &
state % time_levs(1) % state % normalBaroclinicVelocity % isActive) then
         if ((domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % input .and. input_obj % &
stream == STREAM_INPUT) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % restart .and. input_obj % &
stream == STREAM_RESTART) .or. &
             (domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity % ioinfo % sfc .and. input_obj % stream &
== STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % state % time_levs(1) % state % normalBaroclinicVelocity)
         end if

      end if

      if (domain % blocklist % mesh % latCell % isPersistent .and. domain % blocklist % mesh % latCell % isActive) then
         if ((domain % blocklist % mesh % latCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % latCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % latCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % latCell)
         end if

      end if

      if (domain % blocklist % mesh % lonCell % isPersistent .and. domain % blocklist % mesh % lonCell % isActive) then
         if ((domain % blocklist % mesh % lonCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % lonCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % lonCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % lonCell)
         end if

      end if

      if (domain % blocklist % mesh % xCell % isPersistent .and. domain % blocklist % mesh % xCell % isActive) then
         if ((domain % blocklist % mesh % xCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % xCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % xCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % xCell)
         end if

      end if

      if (domain % blocklist % mesh % yCell % isPersistent .and. domain % blocklist % mesh % yCell % isActive) then
         if ((domain % blocklist % mesh % yCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % yCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % yCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % yCell)
         end if

      end if

      if (domain % blocklist % mesh % zCell % isPersistent .and. domain % blocklist % mesh % zCell % isActive) then
         if ((domain % blocklist % mesh % zCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % zCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % zCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % zCell)
         end if

      end if

      if (domain % blocklist % mesh % indexToCellID % isPersistent .and. domain % blocklist % mesh % indexToCellID % isActive) then
         if ((domain % blocklist % mesh % indexToCellID % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % indexToCellID % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % indexToCellID % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % indexToCellID)
         end if

      end if

      if (domain % blocklist % mesh % latEdge % isPersistent .and. domain % blocklist % mesh % latEdge % isActive) then
         if ((domain % blocklist % mesh % latEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % latEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % latEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % latEdge)
         end if

      end if

      if (domain % blocklist % mesh % lonEdge % isPersistent .and. domain % blocklist % mesh % lonEdge % isActive) then
         if ((domain % blocklist % mesh % lonEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % lonEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % lonEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % lonEdge)
         end if

      end if

      if (domain % blocklist % mesh % xEdge % isPersistent .and. domain % blocklist % mesh % xEdge % isActive) then
         if ((domain % blocklist % mesh % xEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % xEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % xEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % xEdge)
         end if

      end if

      if (domain % blocklist % mesh % yEdge % isPersistent .and. domain % blocklist % mesh % yEdge % isActive) then
         if ((domain % blocklist % mesh % yEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % yEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % yEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % yEdge)
         end if

      end if

      if (domain % blocklist % mesh % zEdge % isPersistent .and. domain % blocklist % mesh % zEdge % isActive) then
         if ((domain % blocklist % mesh % zEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % zEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % zEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % zEdge)
         end if

      end if

      if (domain % blocklist % mesh % indexToEdgeID % isPersistent .and. domain % blocklist % mesh % indexToEdgeID % isActive) then
         if ((domain % blocklist % mesh % indexToEdgeID % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % indexToEdgeID % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % indexToEdgeID % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % indexToEdgeID)
         end if

      end if

      if (domain % blocklist % mesh % latVertex % isPersistent .and. domain % blocklist % mesh % latVertex % isActive) then
         if ((domain % blocklist % mesh % latVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % latVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % latVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % latVertex)
         end if

      end if

      if (domain % blocklist % mesh % lonVertex % isPersistent .and. domain % blocklist % mesh % lonVertex % isActive) then
         if ((domain % blocklist % mesh % lonVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % lonVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % lonVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % lonVertex)
         end if

      end if

      if (domain % blocklist % mesh % xVertex % isPersistent .and. domain % blocklist % mesh % xVertex % isActive) then
         if ((domain % blocklist % mesh % xVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % xVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % xVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % xVertex)
         end if

      end if

      if (domain % blocklist % mesh % yVertex % isPersistent .and. domain % blocklist % mesh % yVertex % isActive) then
         if ((domain % blocklist % mesh % yVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % yVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % yVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % yVertex)
         end if

      end if

      if (domain % blocklist % mesh % zVertex % isPersistent .and. domain % blocklist % mesh % zVertex % isActive) then
         if ((domain % blocklist % mesh % zVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % zVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % zVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % zVertex)
         end if

      end if

      if (domain % blocklist % mesh % indexToVertexID % isPersistent .and. domain % blocklist % mesh % indexToVertexID % isActive) &
then
         if ((domain % blocklist % mesh % indexToVertexID % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % indexToVertexID % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % indexToVertexID % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % indexToVertexID)
         end if

      end if

      if (domain % blocklist % mesh % meshDensity % isPersistent .and. domain % blocklist % mesh % meshDensity % isActive) then
         if ((domain % blocklist % mesh % meshDensity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % meshDensity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % meshDensity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % meshDensity)
         end if

      end if

      if (domain % blocklist % mesh % meshScalingDel2 % isPersistent .and. domain % blocklist % mesh % meshScalingDel2 % isActive) &
then
         if ((domain % blocklist % mesh % meshScalingDel2 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % meshScalingDel2 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % meshScalingDel2 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % meshScalingDel2)
         end if

      end if

      if (domain % blocklist % mesh % meshScalingDel4 % isPersistent .and. domain % blocklist % mesh % meshScalingDel4 % isActive) &
then
         if ((domain % blocklist % mesh % meshScalingDel4 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % meshScalingDel4 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % meshScalingDel4 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % meshScalingDel4)
         end if

      end if

      if (domain % blocklist % mesh % meshScaling % isPersistent .and. domain % blocklist % mesh % meshScaling % isActive) then
         if ((domain % blocklist % mesh % meshScaling % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % meshScaling % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % meshScaling % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % meshScaling)
         end if

      end if

      if (domain % blocklist % mesh % cellsOnEdge % isPersistent .and. domain % blocklist % mesh % cellsOnEdge % isActive) then
         if ((domain % blocklist % mesh % cellsOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % cellsOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % cellsOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % cellsOnEdge)
         end if

      end if

      if (domain % blocklist % mesh % nEdgesOnCell % isPersistent .and. domain % blocklist % mesh % nEdgesOnCell % isActive) then
         if ((domain % blocklist % mesh % nEdgesOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % nEdgesOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % nEdgesOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % nEdgesOnCell)
         end if

      end if

      if (domain % blocklist % mesh % nEdgesOnEdge % isPersistent .and. domain % blocklist % mesh % nEdgesOnEdge % isActive) then
         if ((domain % blocklist % mesh % nEdgesOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % nEdgesOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % nEdgesOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % nEdgesOnEdge)
         end if

      end if

      if (domain % blocklist % mesh % edgesOnCell % isPersistent .and. domain % blocklist % mesh % edgesOnCell % isActive) then
         if ((domain % blocklist % mesh % edgesOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgesOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgesOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgesOnCell)
         end if

      end if

      if (domain % blocklist % mesh % edgesOnEdge % isPersistent .and. domain % blocklist % mesh % edgesOnEdge % isActive) then
         if ((domain % blocklist % mesh % edgesOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgesOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgesOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgesOnEdge)
         end if

      end if

      if (domain % blocklist % mesh % weightsOnEdge % isPersistent .and. domain % blocklist % mesh % weightsOnEdge % isActive) then
         if ((domain % blocklist % mesh % weightsOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % weightsOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % weightsOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % weightsOnEdge)
         end if

      end if

      if (domain % blocklist % mesh % dvEdge % isPersistent .and. domain % blocklist % mesh % dvEdge % isActive) then
         if ((domain % blocklist % mesh % dvEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % dvEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % dvEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % dvEdge)
         end if

      end if

      if (domain % blocklist % mesh % dcEdge % isPersistent .and. domain % blocklist % mesh % dcEdge % isActive) then
         if ((domain % blocklist % mesh % dcEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % dcEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % dcEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % dcEdge)
         end if

      end if

      if (domain % blocklist % mesh % angleEdge % isPersistent .and. domain % blocklist % mesh % angleEdge % isActive) then
         if ((domain % blocklist % mesh % angleEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % angleEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % angleEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % angleEdge)
         end if

      end if

      if (domain % blocklist % mesh % areaCell % isPersistent .and. domain % blocklist % mesh % areaCell % isActive) then
         if ((domain % blocklist % mesh % areaCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % areaCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % areaCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % areaCell)
         end if

      end if

      if (domain % blocklist % mesh % areaTriangle % isPersistent .and. domain % blocklist % mesh % areaTriangle % isActive) then
         if ((domain % blocklist % mesh % areaTriangle % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % areaTriangle % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % areaTriangle % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % areaTriangle)
         end if

      end if

      if (domain % blocklist % mesh % edgeNormalVectors % isPersistent .and. domain % blocklist % mesh % edgeNormalVectors % &
isActive) then
         if ((domain % blocklist % mesh % edgeNormalVectors % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgeNormalVectors % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgeNormalVectors % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgeNormalVectors)
         end if

      end if

      if (domain % blocklist % mesh % edgeTangentVectors % isPersistent .and. domain % blocklist % mesh % edgeTangentVectors % &
isActive) then
         if ((domain % blocklist % mesh % edgeTangentVectors % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgeTangentVectors % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgeTangentVectors % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgeTangentVectors)
         end if

      end if

      if (domain % blocklist % mesh % localVerticalUnitVectors % isPersistent .and. domain % blocklist % mesh % &
localVerticalUnitVectors % isActive) then
         if ((domain % blocklist % mesh % localVerticalUnitVectors % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % mesh % localVerticalUnitVectors % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % mesh % localVerticalUnitVectors % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % localVerticalUnitVectors)
         end if

      end if

      if (domain % blocklist % mesh % cellTangentPlane % isPersistent .and. domain % blocklist % mesh % cellTangentPlane % &
isActive) then
         if ((domain % blocklist % mesh % cellTangentPlane % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % cellTangentPlane % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % cellTangentPlane % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % cellTangentPlane)
         end if

      end if

      if (domain % blocklist % mesh % cellsOnCell % isPersistent .and. domain % blocklist % mesh % cellsOnCell % isActive) then
         if ((domain % blocklist % mesh % cellsOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % cellsOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % cellsOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % cellsOnCell)
         end if

      end if

      if (domain % blocklist % mesh % verticesOnCell % isPersistent .and. domain % blocklist % mesh % verticesOnCell % isActive) &
then
         if ((domain % blocklist % mesh % verticesOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % verticesOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % verticesOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % verticesOnCell)
         end if

      end if

      if (domain % blocklist % mesh % verticesOnEdge % isPersistent .and. domain % blocklist % mesh % verticesOnEdge % isActive) &
then
         if ((domain % blocklist % mesh % verticesOnEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % verticesOnEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % verticesOnEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % verticesOnEdge)
         end if

      end if

      if (domain % blocklist % mesh % edgesOnVertex % isPersistent .and. domain % blocklist % mesh % edgesOnVertex % isActive) then
         if ((domain % blocklist % mesh % edgesOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgesOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgesOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgesOnVertex)
         end if

      end if

      if (domain % blocklist % mesh % cellsOnVertex % isPersistent .and. domain % blocklist % mesh % cellsOnVertex % isActive) then
         if ((domain % blocklist % mesh % cellsOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % cellsOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % cellsOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % cellsOnVertex)
         end if

      end if

      if (domain % blocklist % mesh % kiteAreasOnVertex % isPersistent .and. domain % blocklist % mesh % kiteAreasOnVertex % &
isActive) then
         if ((domain % blocklist % mesh % kiteAreasOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % kiteAreasOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % kiteAreasOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % kiteAreasOnVertex)
         end if

      end if

      if (domain % blocklist % mesh % fEdge % isPersistent .and. domain % blocklist % mesh % fEdge % isActive) then
         if ((domain % blocklist % mesh % fEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % fEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % fEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % fEdge)
         end if

      end if

      if (domain % blocklist % mesh % fVertex % isPersistent .and. domain % blocklist % mesh % fVertex % isActive) then
         if ((domain % blocklist % mesh % fVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % fVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % fVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % fVertex)
         end if

      end if

      if (domain % blocklist % mesh % fCell % isPersistent .and. domain % blocklist % mesh % fCell % isActive) then
         if ((domain % blocklist % mesh % fCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % fCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % fCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % fCell)
         end if

      end if

      if (domain % blocklist % mesh % bottomDepth % isPersistent .and. domain % blocklist % mesh % bottomDepth % isActive) then
         if ((domain % blocklist % mesh % bottomDepth % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % bottomDepth % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % bottomDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % bottomDepth)
         end if

      end if

      if (domain % blocklist % mesh % derivTwo % isPersistent .and. domain % blocklist % mesh % derivTwo % isActive) then
         if ((domain % blocklist % mesh % derivTwo % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % derivTwo % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % derivTwo % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % derivTwo)
         end if

      end if

      if (domain % blocklist % mesh % advCoefs % isPersistent .and. domain % blocklist % mesh % advCoefs % isActive) then
         if ((domain % blocklist % mesh % advCoefs % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % advCoefs % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % advCoefs % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % advCoefs)
         end if

      end if

      if (domain % blocklist % mesh % advCoefs3rd % isPersistent .and. domain % blocklist % mesh % advCoefs3rd % isActive) then
         if ((domain % blocklist % mesh % advCoefs3rd % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % advCoefs3rd % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % advCoefs3rd % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % advCoefs3rd)
         end if

      end if

      if (domain % blocklist % mesh % advCellsForEdge % isPersistent .and. domain % blocklist % mesh % advCellsForEdge % isActive) &
then
         if ((domain % blocklist % mesh % advCellsForEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % advCellsForEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % advCellsForEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % advCellsForEdge)
         end if

      end if

      if (domain % blocklist % mesh % nAdvCellsForEdge % isPersistent .and. domain % blocklist % mesh % nAdvCellsForEdge % &
isActive) then
         if ((domain % blocklist % mesh % nAdvCellsForEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % nAdvCellsForEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % nAdvCellsForEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % nAdvCellsForEdge)
         end if

      end if

      if (domain % blocklist % mesh % highOrderAdvectionMask % isPersistent .and. domain % blocklist % mesh % &
highOrderAdvectionMask % isActive) then
         if ((domain % blocklist % mesh % highOrderAdvectionMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % highOrderAdvectionMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % mesh % highOrderAdvectionMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % highOrderAdvectionMask)
         end if

      end if

      if (domain % blocklist % mesh % coeffs_reconstruct % isPersistent .and. domain % blocklist % mesh % coeffs_reconstruct % &
isActive) then
         if ((domain % blocklist % mesh % coeffs_reconstruct % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % coeffs_reconstruct % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % coeffs_reconstruct % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % coeffs_reconstruct)
         end if

      end if

      if (domain % blocklist % mesh % maxLevelCell % isPersistent .and. domain % blocklist % mesh % maxLevelCell % isActive) then
         if ((domain % blocklist % mesh % maxLevelCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % maxLevelCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % maxLevelCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % maxLevelCell)
         end if

      end if

      if (domain % blocklist % mesh % maxLevelEdgeTop % isPersistent .and. domain % blocklist % mesh % maxLevelEdgeTop % isActive) &
then
         if ((domain % blocklist % mesh % maxLevelEdgeTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % maxLevelEdgeTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % maxLevelEdgeTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % maxLevelEdgeTop)
         end if

      end if

      if (domain % blocklist % mesh % maxLevelEdgeBot % isPersistent .and. domain % blocklist % mesh % maxLevelEdgeBot % isActive) &
then
         if ((domain % blocklist % mesh % maxLevelEdgeBot % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % maxLevelEdgeBot % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % maxLevelEdgeBot % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % maxLevelEdgeBot)
         end if

      end if

      if (domain % blocklist % mesh % maxLevelVertexTop % isPersistent .and. domain % blocklist % mesh % maxLevelVertexTop % &
isActive) then
         if ((domain % blocklist % mesh % maxLevelVertexTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % maxLevelVertexTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % maxLevelVertexTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % maxLevelVertexTop)
         end if

      end if

      if (domain % blocklist % mesh % maxLevelVertexBot % isPersistent .and. domain % blocklist % mesh % maxLevelVertexBot % &
isActive) then
         if ((domain % blocklist % mesh % maxLevelVertexBot % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % maxLevelVertexBot % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % maxLevelVertexBot % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % maxLevelVertexBot)
         end if

      end if

      if (domain % blocklist % mesh % boundaryEdge % isPersistent .and. domain % blocklist % mesh % boundaryEdge % isActive) then
         if ((domain % blocklist % mesh % boundaryEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % boundaryEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % boundaryEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % boundaryEdge)
         end if

      end if

      if (domain % blocklist % mesh % boundaryVertex % isPersistent .and. domain % blocklist % mesh % boundaryVertex % isActive) &
then
         if ((domain % blocklist % mesh % boundaryVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % boundaryVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % boundaryVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % boundaryVertex)
         end if

      end if

      if (domain % blocklist % mesh % boundaryCell % isPersistent .and. domain % blocklist % mesh % boundaryCell % isActive) then
         if ((domain % blocklist % mesh % boundaryCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % boundaryCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % boundaryCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % boundaryCell)
         end if

      end if

      if (domain % blocklist % mesh % edgeMask % isPersistent .and. domain % blocklist % mesh % edgeMask % isActive) then
         if ((domain % blocklist % mesh % edgeMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgeMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgeMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgeMask)
         end if

      end if

      if (domain % blocklist % mesh % vertexMask % isPersistent .and. domain % blocklist % mesh % vertexMask % isActive) then
         if ((domain % blocklist % mesh % vertexMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % vertexMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % vertexMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % vertexMask)
         end if

      end if

      if (domain % blocklist % mesh % cellMask % isPersistent .and. domain % blocklist % mesh % cellMask % isActive) then
         if ((domain % blocklist % mesh % cellMask % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % cellMask % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % cellMask % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % cellMask)
         end if

      end if

      if (domain % blocklist % mesh % temperatureRestore % isPersistent .and. domain % blocklist % mesh % temperatureRestore % &
isActive) then
         if ((domain % blocklist % mesh % temperatureRestore % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % temperatureRestore % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % temperatureRestore % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % temperatureRestore)
         end if

      end if

      if (domain % blocklist % mesh % salinityRestore % isPersistent .and. domain % blocklist % mesh % salinityRestore % isActive) &
then
         if ((domain % blocklist % mesh % salinityRestore % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % salinityRestore % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % salinityRestore % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % salinityRestore)
         end if

      end if

      if (domain % blocklist % mesh % edgeSignOnCell % isPersistent .and. domain % blocklist % mesh % edgeSignOnCell % isActive) &
then
         if ((domain % blocklist % mesh % edgeSignOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgeSignOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgeSignOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgeSignOnCell)
         end if

      end if

      if (domain % blocklist % mesh % edgeSignOnVertex % isPersistent .and. domain % blocklist % mesh % edgeSignOnVertex % &
isActive) then
         if ((domain % blocklist % mesh % edgeSignOnVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % edgeSignOnVertex % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % edgeSignOnVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % edgeSignOnVertex)
         end if

      end if

      if (domain % blocklist % mesh % kiteIndexOnCell % isPersistent .and. domain % blocklist % mesh % kiteIndexOnCell % isActive) &
then
         if ((domain % blocklist % mesh % kiteIndexOnCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % kiteIndexOnCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % kiteIndexOnCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % mesh % kiteIndexOnCell)
         end if

      end if

      if (domain % blocklist % verticalMesh % restingThickness % isPersistent .and. domain % blocklist % verticalMesh % &
restingThickness % isActive) then
         if ((domain % blocklist % verticalMesh % restingThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % verticalMesh % restingThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % verticalMesh % restingThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % verticalMesh % restingThickness)
         end if

      end if

      if (domain % blocklist % tend % tracers % isPersistent .and. domain % blocklist % tend % tracers % isActive) then
         if ((domain % blocklist % tend % tracers % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % tend % tracers % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % tend % tracers % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % tend % tracers)
         end if

      end if

      if (domain % blocklist % tend % normalVelocity % isPersistent .and. domain % blocklist % tend % normalVelocity % isActive) &
then
         if ((domain % blocklist % tend % normalVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % tend % normalVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % tend % normalVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % tend % normalVelocity)
         end if

      end if

      if (domain % blocklist % tend % layerThickness % isPersistent .and. domain % blocklist % tend % layerThickness % isActive) &
then
         if ((domain % blocklist % tend % layerThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % tend % layerThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % tend % layerThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % tend % layerThickness)
         end if

      end if

      if (domain % blocklist % tend % ssh % isPersistent .and. domain % blocklist % tend % ssh % isActive) then
         if ((domain % blocklist % tend % ssh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % tend % ssh % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % tend % ssh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % tend % ssh)
         end if

      end if

      if (domain % blocklist % tend % highFreqThickness % isPersistent .and. domain % blocklist % tend % highFreqThickness % &
isActive) then
         if ((domain % blocklist % tend % highFreqThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % tend % highFreqThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % tend % highFreqThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % tend % highFreqThickness)
         end if

      end if

      if (domain % blocklist % tend % lowFreqDivergence % isPersistent .and. domain % blocklist % tend % lowFreqDivergence % &
isActive) then
         if ((domain % blocklist % tend % lowFreqDivergence % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % tend % lowFreqDivergence % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % tend % lowFreqDivergence % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % tend % lowFreqDivergence)
         end if

      end if

      if (domain % blocklist % diagnostics % tracersSurfaceValue % isPersistent .and. domain % blocklist % diagnostics % &
tracersSurfaceValue % isActive) then
         if ((domain % blocklist % diagnostics % tracersSurfaceValue % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % tracersSurfaceValue % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % tracersSurfaceValue % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % tracersSurfaceValue)
         end if

      end if

      if (domain % blocklist % diagnostics % surfaceVelocity % isPersistent .and. domain % blocklist % diagnostics % &
surfaceVelocity % isActive) then
         if ((domain % blocklist % diagnostics % surfaceVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % surfaceVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % surfaceVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % surfaceVelocity)
         end if

      end if

      if (domain % blocklist % diagnostics % SSHGradient % isPersistent .and. domain % blocklist % diagnostics % SSHGradient % &
isActive) then
         if ((domain % blocklist % diagnostics % SSHGradient % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % SSHGradient % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % SSHGradient % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % SSHGradient)
         end if

      end if

      if (domain % blocklist % diagnostics % zMid % isPersistent .and. domain % blocklist % diagnostics % zMid % isActive) then
         if ((domain % blocklist % diagnostics % zMid % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % zMid % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % zMid % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % zMid)
         end if

      end if

      if (domain % blocklist % diagnostics % zTop % isPersistent .and. domain % blocklist % diagnostics % zTop % isActive) then
         if ((domain % blocklist % diagnostics % zTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % zTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % zTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % zTop)
         end if

      end if

      if (domain % blocklist % diagnostics % density % isPersistent .and. domain % blocklist % diagnostics % density % isActive) &
then
         if ((domain % blocklist % diagnostics % density % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % density % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % density % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % density)
         end if

      end if

      if (domain % blocklist % diagnostics % displacedDensity % isPersistent .and. domain % blocklist % diagnostics % &
displacedDensity % isActive) then
         if ((domain % blocklist % diagnostics % displacedDensity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % displacedDensity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % displacedDensity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % displacedDensity)
         end if

      end if

      if (domain % blocklist % diagnostics % potentialDensity % isPersistent .and. domain % blocklist % diagnostics % &
potentialDensity % isActive) then
         if ((domain % blocklist % diagnostics % potentialDensity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % potentialDensity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % potentialDensity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % potentialDensity)
         end if

      end if

      if (domain % blocklist % diagnostics % BruntVaisalaFreqTop % isPersistent .and. domain % blocklist % diagnostics % &
BruntVaisalaFreqTop % isActive) then
         if ((domain % blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % BruntVaisalaFreqTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % BruntVaisalaFreqTop)
         end if

      end if

      if (domain % blocklist % diagnostics % montgomeryPotential % isPersistent .and. domain % blocklist % diagnostics % &
montgomeryPotential % isActive) then
         if ((domain % blocklist % diagnostics % montgomeryPotential % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % montgomeryPotential % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % montgomeryPotential % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % montgomeryPotential)
         end if

      end if

      if (domain % blocklist % diagnostics % pressure % isPersistent .and. domain % blocklist % diagnostics % pressure % isActive) &
then
         if ((domain % blocklist % diagnostics % pressure % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % pressure % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % pressure % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % pressure)
         end if

      end if

      if (domain % blocklist % diagnostics % uTransport % isPersistent .and. domain % blocklist % diagnostics % uTransport % &
isActive) then
         if ((domain % blocklist % diagnostics % uTransport % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % uTransport % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % uTransport % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % uTransport)
         end if

      end if

      if (domain % blocklist % diagnostics % vertTransportVelocityTop % isPersistent .and. domain % blocklist % diagnostics % &
vertTransportVelocityTop % isActive) then
         if ((domain % blocklist % diagnostics % vertTransportVelocityTop % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % vertTransportVelocityTop % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % vertTransportVelocityTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % vertTransportVelocityTop)
         end if

      end if

      if (domain % blocklist % diagnostics % vertVelocityTop % isPersistent .and. domain % blocklist % diagnostics % &
vertVelocityTop % isActive) then
         if ((domain % blocklist % diagnostics % vertVelocityTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % vertVelocityTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % vertVelocityTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % vertVelocityTop)
         end if

      end if

      if (domain % blocklist % diagnostics % tangentialVelocity % isPersistent .and. domain % blocklist % diagnostics % &
tangentialVelocity % isActive) then
         if ((domain % blocklist % diagnostics % tangentialVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % tangentialVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % tangentialVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % tangentialVelocity)
         end if

      end if

      if (domain % blocklist % diagnostics % layerThicknessEdge % isPersistent .and. domain % blocklist % diagnostics % &
layerThicknessEdge % isActive) then
         if ((domain % blocklist % diagnostics % layerThicknessEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % layerThicknessEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % layerThicknessEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % layerThicknessEdge)
         end if

      end if

      if (domain % blocklist % diagnostics % layerThicknessVertex % isPersistent .and. domain % blocklist % diagnostics % &
layerThicknessVertex % isActive) then
         if ((domain % blocklist % diagnostics % layerThicknessVertex % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % layerThicknessVertex % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % layerThicknessVertex % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % layerThicknessVertex)
         end if

      end if

      if (domain % blocklist % diagnostics % kineticEnergyCell % isPersistent .and. domain % blocklist % diagnostics % &
kineticEnergyCell % isActive) then
         if ((domain % blocklist % diagnostics % kineticEnergyCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % kineticEnergyCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % kineticEnergyCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % kineticEnergyCell)
         end if

      end if

      if (domain % blocklist % diagnostics % hEddyFlux % isPersistent .and. domain % blocklist % diagnostics % hEddyFlux % &
isActive) then
         if ((domain % blocklist % diagnostics % hEddyFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % hEddyFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % hEddyFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % hEddyFlux)
         end if

      end if

      if (domain % blocklist % diagnostics % hKappa % isPersistent .and. domain % blocklist % diagnostics % hKappa % isActive) then
         if ((domain % blocklist % diagnostics % hKappa % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % hKappa % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % hKappa % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % hKappa)
         end if

      end if

      if (domain % blocklist % diagnostics % hKappaQ % isPersistent .and. domain % blocklist % diagnostics % hKappaQ % isActive) &
then
         if ((domain % blocklist % diagnostics % hKappaQ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % hKappaQ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % hKappaQ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % hKappaQ)
         end if

      end if

      if (domain % blocklist % diagnostics % viscosity % isPersistent .and. domain % blocklist % diagnostics % viscosity % &
isActive) then
         if ((domain % blocklist % diagnostics % viscosity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % viscosity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % viscosity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % viscosity)
         end if

      end if

      if (domain % blocklist % diagnostics % divergence % isPersistent .and. domain % blocklist % diagnostics % divergence % &
isActive) then
         if ((domain % blocklist % diagnostics % divergence % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % divergence % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % divergence % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % divergence)
         end if

      end if

      if (domain % blocklist % diagnostics % circulation % isPersistent .and. domain % blocklist % diagnostics % circulation % &
isActive) then
         if ((domain % blocklist % diagnostics % circulation % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % circulation % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % circulation % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % circulation)
         end if

      end if

      if (domain % blocklist % diagnostics % relativeVorticity % isPersistent .and. domain % blocklist % diagnostics % &
relativeVorticity % isActive) then
         if ((domain % blocklist % diagnostics % relativeVorticity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % relativeVorticity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % relativeVorticity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % relativeVorticity)
         end if

      end if

      if (domain % blocklist % diagnostics % relativeVorticityCell % isPersistent .and. domain % blocklist % diagnostics % &
relativeVorticityCell % isActive) then
         if ((domain % blocklist % diagnostics % relativeVorticityCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % relativeVorticityCell % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % relativeVorticityCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % relativeVorticityCell)
         end if

      end if

      if (domain % blocklist % diagnostics % normalizedRelativeVorticityEdge % isPersistent .and. domain % blocklist % diagnostics &
% normalizedRelativeVorticityEdge % isActive) then
         if ((domain % blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % normalizedRelativeVorticityEdge % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalizedRelativeVorticityEdge)
         end if

      end if

      if (domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge % isPersistent .and. domain % blocklist % &
diagnostics % normalizedPlanetaryVorticityEdge % isActive) then
         if ((domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalizedPlanetaryVorticityEdge)
         end if

      end if

      if (domain % blocklist % diagnostics % normalizedRelativeVorticityCell % isPersistent .and. domain % blocklist % diagnostics &
% normalizedRelativeVorticityCell % isActive) then
         if ((domain % blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % normalizedRelativeVorticityCell % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalizedRelativeVorticityCell)
         end if

      end if

      if (domain % blocklist % diagnostics % barotropicForcing % isPersistent .and. domain % blocklist % diagnostics % &
barotropicForcing % isActive) then
         if ((domain % blocklist % diagnostics % barotropicForcing % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % barotropicForcing % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % barotropicForcing % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % barotropicForcing)
         end if

      end if

      if (domain % blocklist % diagnostics % barotropicThicknessFlux % isPersistent .and. domain % blocklist % diagnostics % &
barotropicThicknessFlux % isActive) then
         if ((domain % blocklist % diagnostics % barotropicThicknessFlux % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % barotropicThicknessFlux % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % barotropicThicknessFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % barotropicThicknessFlux)
         end if

      end if

      if (domain % blocklist % diagnostics % normalVelocityX % isPersistent .and. domain % blocklist % diagnostics % &
normalVelocityX % isActive) then
         if ((domain % blocklist % diagnostics % normalVelocityX % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % normalVelocityX % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % normalVelocityX % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalVelocityX)
         end if

      end if

      if (domain % blocklist % diagnostics % normalVelocityY % isPersistent .and. domain % blocklist % diagnostics % &
normalVelocityY % isActive) then
         if ((domain % blocklist % diagnostics % normalVelocityY % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % normalVelocityY % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % normalVelocityY % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalVelocityY)
         end if

      end if

      if (domain % blocklist % diagnostics % normalVelocityZ % isPersistent .and. domain % blocklist % diagnostics % &
normalVelocityZ % isActive) then
         if ((domain % blocklist % diagnostics % normalVelocityZ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % normalVelocityZ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % normalVelocityZ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalVelocityZ)
         end if

      end if

      if (domain % blocklist % diagnostics % normalVelocityZonal % isPersistent .and. domain % blocklist % diagnostics % &
normalVelocityZonal % isActive) then
         if ((domain % blocklist % diagnostics % normalVelocityZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % normalVelocityZonal % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % normalVelocityZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalVelocityZonal)
         end if

      end if

      if (domain % blocklist % diagnostics % normalVelocityMeridional % isPersistent .and. domain % blocklist % diagnostics % &
normalVelocityMeridional % isActive) then
         if ((domain % blocklist % diagnostics % normalVelocityMeridional % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % normalVelocityMeridional % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % normalVelocityMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % normalVelocityMeridional)
         end if

      end if

      if (domain % blocklist % diagnostics % gradSSH % isPersistent .and. domain % blocklist % diagnostics % gradSSH % isActive) &
then
         if ((domain % blocklist % diagnostics % gradSSH % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % gradSSH % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % gradSSH % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % gradSSH)
         end if

      end if

      if (domain % blocklist % diagnostics % gradSSHX % isPersistent .and. domain % blocklist % diagnostics % gradSSHX % isActive) &
then
         if ((domain % blocklist % diagnostics % gradSSHX % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % gradSSHX % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % gradSSHX % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % gradSSHX)
         end if

      end if

      if (domain % blocklist % diagnostics % gradSSHY % isPersistent .and. domain % blocklist % diagnostics % gradSSHY % isActive) &
then
         if ((domain % blocklist % diagnostics % gradSSHY % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % gradSSHY % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % gradSSHY % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % gradSSHY)
         end if

      end if

      if (domain % blocklist % diagnostics % gradSSHZ % isPersistent .and. domain % blocklist % diagnostics % gradSSHZ % isActive) &
then
         if ((domain % blocklist % diagnostics % gradSSHZ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % gradSSHZ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % gradSSHZ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % gradSSHZ)
         end if

      end if

      if (domain % blocklist % diagnostics % gradSSHZonal % isPersistent .and. domain % blocklist % diagnostics % gradSSHZonal % &
isActive) then
         if ((domain % blocklist % diagnostics % gradSSHZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % gradSSHZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % gradSSHZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % gradSSHZonal)
         end if

      end if

      if (domain % blocklist % diagnostics % gradSSHMeridional % isPersistent .and. domain % blocklist % diagnostics % &
gradSSHMeridional % isActive) then
         if ((domain % blocklist % diagnostics % gradSSHMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % gradSSHMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % gradSSHMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % gradSSHMeridional)
         end if

      end if

      if (domain % blocklist % diagnostics % uBolusGM % isPersistent .and. domain % blocklist % diagnostics % uBolusGM % isActive) &
then
         if ((domain % blocklist % diagnostics % uBolusGM % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % uBolusGM % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % uBolusGM % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % uBolusGM)
         end if

      end if

      if (domain % blocklist % diagnostics % uBolusGMX % isPersistent .and. domain % blocklist % diagnostics % uBolusGMX % &
isActive) then
         if ((domain % blocklist % diagnostics % uBolusGMX % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % uBolusGMX % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % uBolusGMX % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % uBolusGMX)
         end if

      end if

      if (domain % blocklist % diagnostics % uBolusGMY % isPersistent .and. domain % blocklist % diagnostics % uBolusGMY % &
isActive) then
         if ((domain % blocklist % diagnostics % uBolusGMY % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % uBolusGMY % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % uBolusGMY % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % uBolusGMY)
         end if

      end if

      if (domain % blocklist % diagnostics % uBolusGMZ % isPersistent .and. domain % blocklist % diagnostics % uBolusGMZ % &
isActive) then
         if ((domain % blocklist % diagnostics % uBolusGMZ % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % uBolusGMZ % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % uBolusGMZ % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % uBolusGMZ)
         end if

      end if

      if (domain % blocklist % diagnostics % uBolusGMZonal % isPersistent .and. domain % blocklist % diagnostics % uBolusGMZonal % &
isActive) then
         if ((domain % blocklist % diagnostics % uBolusGMZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % uBolusGMZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % uBolusGMZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % uBolusGMZonal)
         end if

      end if

      if (domain % blocklist % diagnostics % uBolusGMMeridional % isPersistent .and. domain % blocklist % diagnostics % &
uBolusGMMeridional % isActive) then
         if ((domain % blocklist % diagnostics % uBolusGMMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % uBolusGMMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % uBolusGMMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % uBolusGMMeridional)
         end if

      end if

      if (domain % blocklist % diagnostics % RiTopOfCell % isPersistent .and. domain % blocklist % diagnostics % RiTopOfCell % &
isActive) then
         if ((domain % blocklist % diagnostics % RiTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % RiTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % RiTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % RiTopOfCell)
         end if

      end if

      if (domain % blocklist % diagnostics % RiTopOfEdge % isPersistent .and. domain % blocklist % diagnostics % RiTopOfEdge % &
isActive) then
         if ((domain % blocklist % diagnostics % RiTopOfEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % RiTopOfEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % RiTopOfEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % RiTopOfEdge)
         end if

      end if

      if (domain % blocklist % diagnostics % vertViscTopOfEdge % isPersistent .and. domain % blocklist % diagnostics % &
vertViscTopOfEdge % isActive) then
         if ((domain % blocklist % diagnostics % vertViscTopOfEdge % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % vertViscTopOfEdge % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % vertViscTopOfEdge % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % vertViscTopOfEdge)
         end if

      end if

      if (domain % blocklist % diagnostics % vertViscTopOfCell % isPersistent .and. domain % blocklist % diagnostics % &
vertViscTopOfCell % isActive) then
         if ((domain % blocklist % diagnostics % vertViscTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % vertViscTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % vertViscTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % vertViscTopOfCell)
         end if

      end if

      if (domain % blocklist % diagnostics % vertDiffTopOfCell % isPersistent .and. domain % blocklist % diagnostics % &
vertDiffTopOfCell % isActive) then
         if ((domain % blocklist % diagnostics % vertDiffTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % vertDiffTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % vertDiffTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % vertDiffTopOfCell)
         end if

      end if

      if (domain % blocklist % diagnostics % bulkRichardsonNumber % isPersistent .and. domain % blocklist % diagnostics % &
bulkRichardsonNumber % isActive) then
         if ((domain % blocklist % diagnostics % bulkRichardsonNumber % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % bulkRichardsonNumber % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % bulkRichardsonNumber % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % bulkRichardsonNumber)
         end if

      end if

      if (domain % blocklist % diagnostics % boundaryLayerDepth % isPersistent .and. domain % blocklist % diagnostics % &
boundaryLayerDepth % isActive) then
         if ((domain % blocklist % diagnostics % boundaryLayerDepth % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % boundaryLayerDepth % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % boundaryLayerDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % boundaryLayerDepth)
         end if

      end if

      if (domain % blocklist % diagnostics % indexBoundaryLayerDepth % isPersistent .and. domain % blocklist % diagnostics % &
indexBoundaryLayerDepth % isActive) then
         if ((domain % blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % indexBoundaryLayerDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % indexBoundaryLayerDepth)
         end if

      end if

      if (domain % blocklist % diagnostics % surfaceFrictionVelocity % isPersistent .and. domain % blocklist % diagnostics % &
surfaceFrictionVelocity % isActive) then
         if ((domain % blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % surfaceFrictionVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % surfaceFrictionVelocity)
         end if

      end if

      if (domain % blocklist % diagnostics % windStressZonalDiag % isPersistent .and. domain % blocklist % diagnostics % &
windStressZonalDiag % isActive) then
         if ((domain % blocklist % diagnostics % windStressZonalDiag % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % windStressZonalDiag % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % windStressZonalDiag % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % windStressZonalDiag)
         end if

      end if

      if (domain % blocklist % diagnostics % windStressMeridionalDiag % isPersistent .and. domain % blocklist % diagnostics % &
windStressMeridionalDiag % isActive) then
         if ((domain % blocklist % diagnostics % windStressMeridionalDiag % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % windStressMeridionalDiag % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % windStressMeridionalDiag % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % windStressMeridionalDiag)
         end if

      end if

      if (domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL % isPersistent .and. domain % blocklist % diagnostics % &
penetrativeTemperatureFluxOBL % isActive) then
         if ((domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % input .and. input_obj % stream == &
STREAM_INPUT) .or. &
             (domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL % ioinfo % sfc .and. input_obj % stream == &
STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % penetrativeTemperatureFluxOBL)
         end if

      end if

      if (domain % blocklist % diagnostics % buoyancyForcingOBL % isPersistent .and. domain % blocklist % diagnostics % &
buoyancyForcingOBL % isActive) then
         if ((domain % blocklist % diagnostics % buoyancyForcingOBL % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % diagnostics % buoyancyForcingOBL % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % diagnostics % buoyancyForcingOBL % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % diagnostics % buoyancyForcingOBL)
         end if

      end if

      if (domain % blocklist % average % avgSsh % isPersistent .and. domain % blocklist % average % avgSsh % isActive) then
         if ((domain % blocklist % average % avgSsh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % average % avgSsh % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % average % avgSsh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % avgSsh)
         end if

      end if

      if (domain % blocklist % average % varSsh % isPersistent .and. domain % blocklist % average % varSsh % isActive) then
         if ((domain % blocklist % average % varSsh % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % average % varSsh % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % average % varSsh % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % varSsh)
         end if

      end if

      if (domain % blocklist % average % avgVelocityZonal % isPersistent .and. domain % blocklist % average % avgVelocityZonal % &
isActive) then
         if ((domain % blocklist % average % avgVelocityZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % average % avgVelocityZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % average % avgVelocityZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % avgVelocityZonal)
         end if

      end if

      if (domain % blocklist % average % avgVelocityMeridional % isPersistent .and. domain % blocklist % average % &
avgVelocityMeridional % isActive) then
         if ((domain % blocklist % average % avgVelocityMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % average % avgVelocityMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % average % avgVelocityMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % avgVelocityMeridional)
         end if

      end if

      if (domain % blocklist % average % varVelocityZonal % isPersistent .and. domain % blocklist % average % varVelocityZonal % &
isActive) then
         if ((domain % blocklist % average % varVelocityZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % average % varVelocityZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % average % varVelocityZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % varVelocityZonal)
         end if

      end if

      if (domain % blocklist % average % varVelocityMeridional % isPersistent .and. domain % blocklist % average % &
varVelocityMeridional % isActive) then
         if ((domain % blocklist % average % varVelocityMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % average % varVelocityMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % average % varVelocityMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % varVelocityMeridional)
         end if

      end if

      if (domain % blocklist % average % avgNormalVelocity % isPersistent .and. domain % blocklist % average % avgNormalVelocity % &
isActive) then
         if ((domain % blocklist % average % avgNormalVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % average % avgNormalVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % average % avgNormalVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % avgNormalVelocity)
         end if

      end if

      if (domain % blocklist % average % varNormalVelocity % isPersistent .and. domain % blocklist % average % varNormalVelocity % &
isActive) then
         if ((domain % blocklist % average % varNormalVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % average % varNormalVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % average % varNormalVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % varNormalVelocity)
         end if

      end if

      if (domain % blocklist % average % avgVertVelocityTop % isPersistent .and. domain % blocklist % average % avgVertVelocityTop &
% isActive) then
         if ((domain % blocklist % average % avgVertVelocityTop % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % average % avgVertVelocityTop % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % average % avgVertVelocityTop % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % average % avgVertVelocityTop)
         end if

      end if

      if (domain % blocklist % forcing % surfaceTracerFlux % isPersistent .and. domain % blocklist % forcing % surfaceTracerFlux % &
isActive) then
         if ((domain % blocklist % forcing % surfaceTracerFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % surfaceTracerFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % surfaceTracerFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % surfaceTracerFlux)
         end if

      end if

      if (domain % blocklist % forcing % avgTracersSurfaceValue % isPersistent .and. domain % blocklist % forcing % &
avgTracersSurfaceValue % isActive) then
         if ((domain % blocklist % forcing % avgTracersSurfaceValue % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % forcing % avgTracersSurfaceValue % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % avgTracersSurfaceValue % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % avgTracersSurfaceValue)
         end if

      end if

      if (domain % blocklist % forcing % avgSurfaceVelocity % isPersistent .and. domain % blocklist % forcing % avgSurfaceVelocity &
% isActive) then
         if ((domain % blocklist % forcing % avgSurfaceVelocity % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % avgSurfaceVelocity % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % avgSurfaceVelocity % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % avgSurfaceVelocity)
         end if

      end if

      if (domain % blocklist % forcing % avgSSHGradient % isPersistent .and. domain % blocklist % forcing % avgSSHGradient % &
isActive) then
         if ((domain % blocklist % forcing % avgSSHGradient % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % avgSSHGradient % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % avgSSHGradient % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % avgSSHGradient)
         end if

      end if

      if (domain % blocklist % forcing % surfaceWindStress % isPersistent .and. domain % blocklist % forcing % surfaceWindStress % &
isActive) then
         if ((domain % blocklist % forcing % surfaceWindStress % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % surfaceWindStress % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % surfaceWindStress % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % surfaceWindStress)
         end if

      end if

      if (domain % blocklist % forcing % surfaceWindStressMagnitude % isPersistent .and. domain % blocklist % forcing % &
surfaceWindStressMagnitude % isActive) then
         if ((domain % blocklist % forcing % surfaceWindStressMagnitude % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % forcing % surfaceWindStressMagnitude % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % forcing % surfaceWindStressMagnitude % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % surfaceWindStressMagnitude)
         end if

      end if

      if (domain % blocklist % forcing % surfaceMassFlux % isPersistent .and. domain % blocklist % forcing % surfaceMassFlux % &
isActive) then
         if ((domain % blocklist % forcing % surfaceMassFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % surfaceMassFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % surfaceMassFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % surfaceMassFlux)
         end if

      end if

      if (domain % blocklist % forcing % seaSurfacePressure % isPersistent .and. domain % blocklist % forcing % seaSurfacePressure &
% isActive) then
         if ((domain % blocklist % forcing % seaSurfacePressure % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % seaSurfacePressure % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % seaSurfacePressure % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % seaSurfacePressure)
         end if

      end if

      if (domain % blocklist % forcing % seaIceEnergy % isPersistent .and. domain % blocklist % forcing % seaIceEnergy % isActive) &
then
         if ((domain % blocklist % forcing % seaIceEnergy % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % seaIceEnergy % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % seaIceEnergy % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % seaIceEnergy)
         end if

      end if

      if (domain % blocklist % forcing % penetrativeTemperatureFlux % isPersistent .and. domain % blocklist % forcing % &
penetrativeTemperatureFlux % isActive) then
         if ((domain % blocklist % forcing % penetrativeTemperatureFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % forcing % penetrativeTemperatureFlux % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % forcing % penetrativeTemperatureFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) &
then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % penetrativeTemperatureFlux)
         end if

      end if

      if (domain % blocklist % forcing % transmissionCoefficients % isPersistent .and. domain % blocklist % forcing % &
transmissionCoefficients % isActive) then
         if ((domain % blocklist % forcing % transmissionCoefficients % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % forcing % transmissionCoefficients % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % forcing % transmissionCoefficients % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % transmissionCoefficients)
         end if

      end if

      if (domain % blocklist % forcing % windStressZonal % isPersistent .and. domain % blocklist % forcing % windStressZonal % &
isActive) then
         if ((domain % blocklist % forcing % windStressZonal % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % windStressZonal % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % windStressZonal % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % windStressZonal)
         end if

      end if

      if (domain % blocklist % forcing % windStressMeridional % isPersistent .and. domain % blocklist % forcing % &
windStressMeridional % isActive) then
         if ((domain % blocklist % forcing % windStressMeridional % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % windStressMeridional % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % windStressMeridional % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % windStressMeridional)
         end if

      end if

      if (domain % blocklist % forcing % latentHeatFlux % isPersistent .and. domain % blocklist % forcing % latentHeatFlux % &
isActive) then
         if ((domain % blocklist % forcing % latentHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % latentHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % latentHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % latentHeatFlux)
         end if

      end if

      if (domain % blocklist % forcing % sensibleHeatFlux % isPersistent .and. domain % blocklist % forcing % sensibleHeatFlux % &
isActive) then
         if ((domain % blocklist % forcing % sensibleHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % sensibleHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % sensibleHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % sensibleHeatFlux)
         end if

      end if

      if (domain % blocklist % forcing % longWaveHeatFluxUp % isPersistent .and. domain % blocklist % forcing % longWaveHeatFluxUp &
% isActive) then
         if ((domain % blocklist % forcing % longWaveHeatFluxUp % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % longWaveHeatFluxUp % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % longWaveHeatFluxUp % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % longWaveHeatFluxUp)
         end if

      end if

      if (domain % blocklist % forcing % longWaveHeatFluxDown % isPersistent .and. domain % blocklist % forcing % &
longWaveHeatFluxDown % isActive) then
         if ((domain % blocklist % forcing % longWaveHeatFluxDown % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % longWaveHeatFluxDown % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % longWaveHeatFluxDown % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % longWaveHeatFluxDown)
         end if

      end if

      if (domain % blocklist % forcing % seaIceHeatFlux % isPersistent .and. domain % blocklist % forcing % seaIceHeatFlux % &
isActive) then
         if ((domain % blocklist % forcing % seaIceHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % seaIceHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % seaIceHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % seaIceHeatFlux)
         end if

      end if

      if (domain % blocklist % forcing % shortWaveHeatFlux % isPersistent .and. domain % blocklist % forcing % shortWaveHeatFlux % &
isActive) then
         if ((domain % blocklist % forcing % shortWaveHeatFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % shortWaveHeatFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % shortWaveHeatFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % shortWaveHeatFlux)
         end if

      end if

      if (domain % blocklist % forcing % evaporationFlux % isPersistent .and. domain % blocklist % forcing % evaporationFlux % &
isActive) then
         if ((domain % blocklist % forcing % evaporationFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % evaporationFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % evaporationFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % evaporationFlux)
         end if

      end if

      if (domain % blocklist % forcing % seaIceSalinityFlux % isPersistent .and. domain % blocklist % forcing % seaIceSalinityFlux &
% isActive) then
         if ((domain % blocklist % forcing % seaIceSalinityFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % seaIceSalinityFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % seaIceSalinityFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % seaIceSalinityFlux)
         end if

      end if

      if (domain % blocklist % forcing % seaIceFreshWaterFlux % isPersistent .and. domain % blocklist % forcing % &
seaIceFreshWaterFlux % isActive) then
         if ((domain % blocklist % forcing % seaIceFreshWaterFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % seaIceFreshWaterFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % forcing % seaIceFreshWaterFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % seaIceFreshWaterFlux)
         end if

      end if

      if (domain % blocklist % forcing % riverRunoffFlux % isPersistent .and. domain % blocklist % forcing % riverRunoffFlux % &
isActive) then
         if ((domain % blocklist % forcing % riverRunoffFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % riverRunoffFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % riverRunoffFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % riverRunoffFlux)
         end if

      end if

      if (domain % blocklist % forcing % iceRunoffFlux % isPersistent .and. domain % blocklist % forcing % iceRunoffFlux % &
isActive) then
         if ((domain % blocklist % forcing % iceRunoffFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % iceRunoffFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % iceRunoffFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % iceRunoffFlux)
         end if

      end if

      if (domain % blocklist % forcing % rainFlux % isPersistent .and. domain % blocklist % forcing % rainFlux % isActive) then
         if ((domain % blocklist % forcing % rainFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % rainFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % rainFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % rainFlux)
         end if

      end if

      if (domain % blocklist % forcing % snowFlux % isPersistent .and. domain % blocklist % forcing % snowFlux % isActive) then
         if ((domain % blocklist % forcing % snowFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % snowFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % snowFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % snowFlux)
         end if

      end if

      if (domain % blocklist % forcing % iceFraction % isPersistent .and. domain % blocklist % forcing % iceFraction % isActive) &
then
         if ((domain % blocklist % forcing % iceFraction % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % iceFraction % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % iceFraction % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % iceFraction)
         end if

      end if

      if (domain % blocklist % forcing % prognosticCO2 % isPersistent .and. domain % blocklist % forcing % prognosticCO2 % &
isActive) then
         if ((domain % blocklist % forcing % prognosticCO2 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % prognosticCO2 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % prognosticCO2 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % prognosticCO2)
         end if

      end if

      if (domain % blocklist % forcing % diagnosticCO2 % isPersistent .and. domain % blocklist % forcing % diagnosticCO2 % &
isActive) then
         if ((domain % blocklist % forcing % diagnosticCO2 % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % diagnosticCO2 % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % diagnosticCO2 % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % diagnosticCO2)
         end if

      end if

      if (domain % blocklist % forcing % squaredWindSpeed10Meter % isPersistent .and. domain % blocklist % forcing % &
squaredWindSpeed10Meter % isActive) then
         if ((domain % blocklist % forcing % squaredWindSpeed10Meter % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % forcing % squaredWindSpeed10Meter % ioinfo % restart .and. input_obj % stream == &
STREAM_RESTART) .or. &
             (domain % blocklist % forcing % squaredWindSpeed10Meter % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % squaredWindSpeed10Meter)
         end if

      end if

      if (domain % blocklist % forcing % CO2Flux % isPersistent .and. domain % blocklist % forcing % CO2Flux % isActive) then
         if ((domain % blocklist % forcing % CO2Flux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % CO2Flux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % CO2Flux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % CO2Flux)
         end if

      end if

      if (domain % blocklist % forcing % DMSFlux % isPersistent .and. domain % blocklist % forcing % DMSFlux % isActive) then
         if ((domain % blocklist % forcing % DMSFlux % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % forcing % DMSFlux % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % forcing % DMSFlux % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % forcing % DMSFlux)
         end if

      end if

      if (domain % blocklist % scratch % thermalExpansionCoeff % isPersistent .and. domain % blocklist % scratch % &
thermalExpansionCoeff % isActive) then
         if ((domain % blocklist % scratch % thermalExpansionCoeff % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % scratch % thermalExpansionCoeff % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % scratch % thermalExpansionCoeff % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % scratch % thermalExpansionCoeff)
         end if

      end if

      if (domain % blocklist % scratch % salineContractionCoeff % isPersistent .and. domain % blocklist % scratch % &
salineContractionCoeff % isActive) then
         if ((domain % blocklist % scratch % salineContractionCoeff % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % scratch % salineContractionCoeff % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % scratch % salineContractionCoeff % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
            call mpas_dmpar_exch_halo_field(domain % blocklist % scratch % salineContractionCoeff)
         end if

      end if


      if (domain % blocklist % mesh % refBottomDepth % isPersistent .and. domain % blocklist % mesh % refBottomDepth % isActive) &
then
         if ((domain % blocklist % mesh % refBottomDepth % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % refBottomDepth % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % mesh % refBottomDepth % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
             call mpas_dmpar_copy_field(domain % blocklist % mesh % refBottomDepth)
         end if

      end if

      if (domain % blocklist % mesh % refBottomDepthTopOfCell % isPersistent .and. domain % blocklist % mesh % &
refBottomDepthTopOfCell % isActive) then
         if ((domain % blocklist % mesh % refBottomDepthTopOfCell % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % mesh % refBottomDepthTopOfCell % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % mesh % refBottomDepthTopOfCell % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
             call mpas_dmpar_copy_field(domain % blocklist % mesh % refBottomDepthTopOfCell)
         end if

      end if

      if (domain % blocklist % mesh % vertCoordMovementWeights % isPersistent .and. domain % blocklist % mesh % &
vertCoordMovementWeights % isActive) then
         if ((domain % blocklist % mesh % vertCoordMovementWeights % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % mesh % vertCoordMovementWeights % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % mesh % vertCoordMovementWeights % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
             call mpas_dmpar_copy_field(domain % blocklist % mesh % vertCoordMovementWeights)
         end if

      end if

      if (domain % blocklist % verticalMesh % refZMid % isPersistent .and. domain % blocklist % verticalMesh % refZMid % isActive) &
then
         if ((domain % blocklist % verticalMesh % refZMid % ioinfo % input .and. input_obj % stream == STREAM_INPUT) .or. &
             (domain % blocklist % verticalMesh % refZMid % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) .or. &
             (domain % blocklist % verticalMesh % refZMid % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
             call mpas_dmpar_copy_field(domain % blocklist % verticalMesh % refZMid)
         end if

      end if

      if (domain % blocklist % verticalMesh % refLayerThickness % isPersistent .and. domain % blocklist % verticalMesh % &
refLayerThickness % isActive) then
         if ((domain % blocklist % verticalMesh % refLayerThickness % ioinfo % input .and. input_obj % stream == STREAM_INPUT) &
.or. &
             (domain % blocklist % verticalMesh % refLayerThickness % ioinfo % restart .and. input_obj % stream == STREAM_RESTART) &
.or. &
             (domain % blocklist % verticalMesh % refLayerThickness % ioinfo % sfc .and. input_obj % stream == STREAM_SFC)) then
             call mpas_dmpar_copy_field(domain % blocklist % verticalMesh % refLayerThickness)
         end if

      end if


   end subroutine mpas_exch_input_field_halos!}}}

   subroutine mpas_io_input_finalize(input_obj, dminfo)!{{{
 
      implicit none
 
      type (io_input_object), intent(inout) :: input_obj
      type (dm_info), intent(in) :: dminfo

      integer :: nferr
 
      call MPAS_closeStream(input_obj % io_stream, nferr)
 
   end subroutine mpas_io_input_finalize!}}}

   subroutine mpas_io_setup_cell_block_fields(inputHandle, nReadCells, readCellStart, readingBlock, maxEdges, indexToCellID, xCell, yCell, zCell, nEdgesOnCell, cellsOnCell, edgesOnCell, verticesOnCell)!{{{
     type (MPAS_IO_Handle_type) :: inputHandle
     integer, intent(in) :: nReadCells
     integer, intent(in) :: readCellStart
     integer, intent(in) :: maxEdges
     type (block_type), pointer :: readingBlock
     type (field1dInteger), pointer :: indexToCellID
     type (field1dReal), pointer :: xCell
     type (field1dReal), pointer :: yCell
     type (field1dReal), pointer :: zCell
     type (field1dInteger), pointer :: nEdgesOnCell
     type (field2dInteger), pointer :: cellsOnCell
     type (field2dInteger), pointer :: edgesOnCell
     type (field2dInteger), pointer :: verticesOnCell

     integer :: i, nHalos
     integer, dimension(:), pointer :: readIndices

     nHalos = config_num_halos
  
     !
     ! Allocate and read fields that we will need in order to ultimately work out
     !   which cells/edges/vertices are owned by each block, and which are ghost
     !

     ! Global cell indices
     allocate(indexToCellID)
     allocate(indexToCellID % ioinfo)
     indexToCellID % ioinfo % fieldName = 'indexToCellID'
     indexToCellID % ioinfo % start(1) = readCellStart
     indexToCellID % ioinfo % count(1) = nReadCells
     allocate(indexToCellID % array(nReadCells))
     allocate(readIndices(nReadCells))
     do i=1,nReadCells
        readIndices(i) = i + readCellStart - 1
     end do
     call MPAS_io_inq_var(inputHandle, 'indexToCellID', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'indexToCellID', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'indexToCellID', indexToCellID % array, ierr)
     indexToCellID % dimSizes(1) = nReadCells
     indexToCellID % block => readingBlock
     call mpas_dmpar_init_multihalo_exchange_list(indexToCellID % sendList, nHalos)
     call mpas_dmpar_init_multihalo_exchange_list(indexToCellID % recvList, nHalos)
     call mpas_dmpar_init_multihalo_exchange_list(indexToCellID % copyList, nHalos)
     nullify(indexToCellID % next)
   

     ! Number of cell/edges/vertices adjacent to each cell
     allocate(nEdgesOnCell)
     allocate(nEdgesOnCell % ioinfo)
     nEdgesOnCell % ioinfo % fieldName = 'nEdgesOnCell'
     nEdgesOnCell % ioinfo % start(1) = readCellStart
     nEdgesOnCell % ioinfo % count(1) = nReadCells
     allocate(nEdgesOnCell % array(nReadCells))
     call MPAS_io_inq_var(inputHandle, 'nEdgesOnCell', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'nEdgesOnCell', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'nEdgesOnCell', nEdgesOnCell % array, ierr)
     nEdgesOnCell % dimSizes(1) = nReadCells
     nEdgesOnCell % block => readingBlock
     nEdgesOnCell % sendList => indexToCellID % sendList
     nEdgesOnCell % recvList => indexToCellID % recvList
     nEdgesOnCell % copyList => indexToCellID % copyList
     nullify(nEdgesOnCell % next)
   
     ! Global indices of cells adjacent to each cell
     allocate(cellsOnCell)
     allocate(cellsOnCell % ioinfo)
     cellsOnCell % ioinfo % fieldName = 'cellsOnCell'
     cellsOnCell % ioinfo % start(1) = 1
     cellsOnCell % ioinfo % start(2) = readCellStart
     cellsOnCell % ioinfo % count(1) = maxEdges
     cellsOnCell % ioinfo % count(2) = nReadCells
     allocate(cellsOnCell % array(maxEdges,nReadCells))
     call MPAS_io_inq_var(inputHandle, 'cellsOnCell', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'cellsOnCell', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'cellsOnCell', cellsOnCell % array, ierr)
     cellsOnCell % dimSizes(1) = maxEdges
     cellsOnCell % dimSizes(2) = nReadCells
     cellsOnCell % block => readingBlock
     cellsOnCell % sendList => indexToCellID % sendList
     cellsOnCell % recvList => indexToCellID % recvList
     cellsOnCell % copyList => indexToCellID % copyList
     nullify(cellsOnCell % next)
   
     ! Global indices of edges adjacent to each cell
     allocate(edgesOnCell)
     allocate(edgesOnCell % ioinfo)
     edgesOnCell % ioinfo % fieldName = 'edgesOnCell'
     edgesOnCell % ioinfo % start(1) = 1
     edgesOnCell % ioinfo % start(2) = readCellStart
     edgesOnCell % ioinfo % count(1) = maxEdges
     edgesOnCell % ioinfo % count(2) = nReadCells
     allocate(edgesOnCell % array(maxEdges,nReadCells))
     call MPAS_io_inq_var(inputHandle, 'edgesOnCell', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'edgesOnCell', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'edgesOnCell', edgesOnCell % array, ierr)
     edgesOnCell % dimSizes(1) = maxEdges
     edgesOnCell % dimSizes(2) = nReadCells
     edgesOnCell % block => readingBlock
     edgesOnCell % sendList => indexToCellID % sendList
     edgesOnCell % recvList => indexToCellID % recvList
     edgesOnCell % copyList => indexToCellID % copyList
     nullify(edgesOnCell % next)
   
     ! Global indices of vertices adjacent to each cell
     allocate(verticesOnCell)
     allocate(verticesOnCell % ioinfo)
     verticesOnCell % ioinfo % fieldName = 'verticesOnCell'
     verticesOnCell % ioinfo % start(1) = 1
     verticesOnCell % ioinfo % start(2) = readCellStart
     verticesOnCell % ioinfo % count(1) = maxEdges
     verticesOnCell % ioinfo % count(2) = nReadCells
     allocate(verticesOnCell % array(maxEdges,nReadCells))
     call MPAS_io_inq_var(inputHandle, 'verticesOnCell', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'verticesOnCell', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'verticesOnCell', verticesOnCell % array, ierr)
     verticesOnCell % dimSizes(1) = maxEdges
     verticesOnCell % dimSizes(2) = nReadCells
     verticesOnCell % block => readingBlock
     verticesOnCell % sendList => indexToCellID % sendList
     verticesOnCell % recvList => indexToCellID % recvList
     verticesOnCell % copyList => indexToCellID % copyList
     nullify(verticesOnCell % next)

     deallocate(readIndices)
   
   end subroutine mpas_io_setup_cell_block_fields!}}}

   subroutine mpas_io_setup_edge_block_fields(inputHandle, nReadEdges, readEdgeStart, readingBlock, indexToEdgeID, xEdge, yEdge, zEdge, cellsOnEdge)!{{{
     type (MPAS_IO_Handle_type) :: inputHandle
     integer, intent(in) :: nReadEdges
     integer, intent(in) :: readEdgeStart
     type (block_type), pointer :: readingBlock
     type (field1dInteger), pointer :: indexToEdgeID
     type (field1dReal), pointer :: xEdge
     type (field1dReal), pointer :: yEdge
     type (field1dReal), pointer :: zEdge
     type (field2dInteger), pointer :: cellsOnEdge

     integer :: i, nHalos
     integer, dimension(:), pointer :: readIndices

     nHalos = config_num_halos
  
     !
     ! Allocate and read fields that we will need in order to ultimately work out
     !   which cells/edges/vertices are owned by each block, and which are ghost
     !

     allocate(readIndices(nReadEdges))

     ! Global edge indices
     allocate(indexToEdgeID)
     allocate(indexToEdgeID % ioinfo)
     indexToEdgeID % ioinfo % fieldName = 'indexToEdgeID'
     indexToEdgeID % ioinfo % start(1) = readEdgeStart
     indexToEdgeID % ioinfo % count(1) = nReadEdges
     allocate(indexToEdgeID % array(nReadEdges))
     allocate(indexToEdgeID % array(nReadEdges))
     do i=1,nReadEdges
        readIndices(i) = i + readEdgeStart - 1
     end do
     call MPAS_io_inq_var(inputHandle, 'indexToEdgeID', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'indexToEdgeID', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'indexToEdgeID', indexToEdgeID % array, ierr)
     indexToEdgeID % dimSizes(1) = nREadEdges
     indexToEdgeID % block => readingBlock
     call mpas_dmpar_init_multihalo_exchange_list(indexToEdgeID % sendList, nHalos+1)
     call mpas_dmpar_init_multihalo_exchange_list(indexToEdgeID % recvList, nHalos+1)
     call mpas_dmpar_init_multihalo_exchange_list(indexToEdgeID % copyList, nHalos+1)
     nullify(indexToEdgeID % next)
   
   
     ! Global indices of cells adjacent to each edge
     !    used for determining which edges are owned by a block, where 
     !    iEdge is owned iff cellsOnEdge(1,iEdge) is an owned cell
     allocate(cellsOnEdge)
     allocate(cellsOnEdge % ioinfo)
     cellsOnEdge % ioinfo % fieldName = 'cellsOnEdge'
     cellsOnEdge % ioinfo % start(1) = 1
     cellsOnEdge % ioinfo % start(2) = readEdgeStart
     cellsOnEdge % ioinfo % count(1) = 2
     cellsOnEdge % ioinfo % count(2) = nReadEdges
     allocate(cellsOnEdge % array(2,nReadEdges))
     call MPAS_io_inq_var(inputHandle, 'cellsOnEdge', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'cellsOnEdge', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'cellsOnEdge', cellsOnEdge % array, ierr)
     cellsOnEdge % dimSizes(1) = 2
     cellsOnEdge % dimSizes(2) = nReadEdges
     cellsOnEdge % block => readingBlock
     cellsOnEdge % sendList => indexToEdgeID % sendList
     cellsOnEdge % recvList => indexToEdgeID % recvList
     cellsOnEdge % copyList => indexToEdgeID % copyList
     nullify(cellsOnEdge % next)

     deallocate(readIndices)
   
   end subroutine mpas_io_setup_edge_block_fields!}}}

   subroutine mpas_io_setup_vertex_block_fields(inputHandle, nReadVertices, readVertexStart, readingBlock, vertexDegree, indexToVertexID, xVertex, yVertex, zVertex, cellsOnVertex)!{{{
     type (MPAS_IO_Handle_type) :: inputHandle
     integer, intent(in) :: nReadVertices
     integer, intent(in) :: readVertexStart
     integer, intent(in) :: vertexDegree
     type (block_type), pointer :: readingBlock
     type (field1dInteger), pointer :: indexToVertexID
     type (field1dReal), pointer :: xVertex
     type (field1dReal), pointer :: yVertex
     type (field1dReal), pointer :: zVertex
     type (field2dInteger), pointer :: cellsOnVertex

     integer :: i, nHalos
     integer, dimension(:), pointer :: readIndices

     nHalos = config_num_halos
  
     ! Global vertex indices
     allocate(indexToVertexID)
     allocate(indexToVertexID % ioinfo)
     indexToVertexID % ioinfo % fieldName = 'indexToVertexID'
     indexToVertexID % ioinfo % start(1) = readVertexStart
     indexToVertexID % ioinfo % count(1) = nReadVertices
     allocate(indexToVertexID % array(nReadVertices))
     allocate(readIndices(nReadVertices))
     do i=1,nReadVertices
        readIndices(i) = i + readVertexStart - 1
     end do
     call MPAS_io_inq_var(inputHandle, 'indexToVertexID', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'indexToVertexID', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'indexToVertexID', indexToVertexID % array, ierr)
     indexToVertexID % dimSizes(1) = nReadVertices
     indexToVertexID % block => readingBlock
     call mpas_dmpar_init_multihalo_exchange_list(indexToVertexID % sendList, nHalos+1)
     call mpas_dmpar_init_multihalo_exchange_list(indexToVertexID % recvList, nHalos+1)
     call mpas_dmpar_init_multihalo_exchange_list(indexToVertexID % copyList, nHalos+1)
     nullify(indexToVertexID % next)
   
   
     ! Global indices of cells adjacent to each vertex
     !    used for determining which vertices are owned by a block, where 
     !    iVtx is owned iff cellsOnVertex(1,iVtx) is an owned cell
     allocate(cellsOnVertex)
     allocate(cellsOnVertex % ioinfo)
     cellsOnVertex % ioinfo % fieldName = 'cellsOnVertex'
     cellsOnVertex % ioinfo % start(1) = 1
     cellsOnVertex % ioinfo % start(2) = readVertexStart
     cellsOnVertex % ioinfo % count(1) = vertexDegree
     cellsOnVertex % ioinfo % count(2) = nReadVertices
     allocate(cellsOnVertex % array(vertexDegree,nReadVertices))
     call MPAS_io_inq_var(inputHandle, 'cellsOnVertex', ierr=ierr)
     call MPAS_io_set_var_indices(inputHandle, 'cellsOnVertex', readIndices, ierr=ierr)
     call mpas_io_get_var(inputHandle, 'cellsOnVertex', cellsOnVertex % array, ierr)
     cellsOnVertex % dimSizes(1) = vertexDegree
     cellsOnVertex % dimSizes(2) = nReadVertices
     cellsOnVertex % block => readingBlock
     cellsOnVertex % sendList => indexToVertexID % sendList
     cellsOnVertex % recvList => indexToVertexID % recvList
     cellsOnVertex % copyList => indexToVertexID % copyList
     nullify(cellsOnVertex % next)

     deallocate(readIndices)

   end subroutine mpas_io_setup_vertex_block_fields!}}}

 
end module mpas_io_input

#ifndef FortranCInterface_HEADER_INCLUDED
#define FortranCInterface_HEADER_INCLUDED

/* Mangling for Fortran global symbols without underscores. */
#define FortranCInterface_GLOBAL(name,NAME) name##_

/* Mangling for Fortran global symbols with underscores. */
#define FortranCInterface_GLOBAL_(name,NAME) name##_

/* Mangling for Fortran module symbols without underscores. */
#define FortranCInterface_MODULE(mod_name,name, mod_NAME,NAME) __##mod_name##_MOD_##name

/* Mangling for Fortran module symbols with underscores. */
#define FortranCInterface_MODULE_(mod_name,name, mod_NAME,NAME) __##mod_name##_MOD_##name

/*--------------------------------------------------------------------------*/
/* Mangle some symbols automatically.                                       */
#define coprocessor_create_grid FortranCInterface_GLOBAL_(coprocessor_create_grid, COPROCESSOR_CREATE_GRID)
#define coprocessor_register_tracer_data FortranCInterface_GLOBAL_(coprocessor_register_tracer_data, COPROCESSOR_REGISTER_TRACER_DATA)
#define coprocessor_add_tracer_data FortranCInterface_GLOBAL_(coprocessor_add_tracer_data, COPROCESSOR_ADD_TRACER_DATA)
#define coprocessor_register_data FortranCInterface_GLOBAL_(coprocessor_register_data, COPROCESSOR_REGISTER_DATA)
#define coprocessor_add_data FortranCInterface_GLOBAL_(coprocessor_add_data, COPROCESSOR_ADD_DATA)
#define mpas_initialize FortranCInterface_GLOBAL_(mpas_initialize, MPAS_INITIALIZE)
#define mpas_registerdata FortranCInterface_GLOBAL_(mpas_registerdata, MPAS_REGISTERDATA)
#define mpas_coprocess FortranCInterface_GLOBAL_(mpas_coprocess, MPAS_COPROCESS)
#define mpas_finalize FortranCInterface_GLOBAL_(mpas_finalize, MPAS_FINALIZE)
#define mpas_check_coprocess FortranCInterface_GLOBAL_(mpas_check_coprocess, MPAS_CHECK_COPROCESS)

#endif

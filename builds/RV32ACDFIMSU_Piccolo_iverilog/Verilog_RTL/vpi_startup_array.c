/*
 * Generated by Bluespec Compiler, version 2021.12.1 (build fd50140)
 * 
 * On Fri May 20 12:28:58 EDT 2022
 * 
 */

/* registration array */
#include "vpi_wrapper_c_end_timing.h"
#include "vpi_wrapper_c_get_symbol_val.h"
#include "vpi_wrapper_c_start_timing.h"
#include "vpi_wrapper_c_trygetchar.h"

/* Convenience function to register all imported functions */
void vpi_register_all()
{
  c_end_timing_vpi_register();
  c_get_symbol_val_vpi_register();
  c_start_timing_vpi_register();
  c_trygetchar_vpi_register();
}

/* Convenience function to register only tasks */
void vpi_register_tasks()
{
  c_end_timing_vpi_register();
  c_start_timing_vpi_register();
}
void (*vlog_startup_routines[])() = { vpi_register_all, 0u };

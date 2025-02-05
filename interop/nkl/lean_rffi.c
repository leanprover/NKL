/*
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
*/
#include <lean/lean.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>

// -----------------------------------------------------------------------------
// Lean part

extern void lean_initialize_runtime_module();
extern lean_object* initialize_KLR(uint8_t builtin, lean_object*);
extern lean_object* parse_json(lean_object*, lean_object*);
extern lean_object* lean_io_error_to_string(lean_object * err);

static lean_object *world = NULL;

int lean_init() {
  lean_initialize_runtime_module();
  world = lean_io_mk_world();
  lean_object *res = initialize_KLR(1, world);
  if (!lean_io_result_is_ok(res)) {
    lean_io_result_show_error(res);
    lean_dec(res);
    return 1;
  }
  lean_dec_ref(res);
  lean_io_mark_end_initialization();
  return 0;
}

int parse(const char *json) {
  // note: lean_mk_string will copy the string
  lean_object *s = lean_mk_string(json);
  lean_object *res = parse_json(s, world);
  if (!lean_io_result_is_ok(res)) {
    lean_object *err = lean_io_result_get_error(res);
    lean_inc(err);
    lean_object *str_obj = lean_io_error_to_string(err);
    const char *str = lean_string_cstr(str_obj);
    Py_INCREF(PyExc_Exception);
    // Note: SetString will copy the string (I think?)
    PyErr_SetString(PyExc_Exception, str);
    lean_dec(str_obj);
    lean_dec(res);
    return 1;
  }
  lean_dec(res);
  return 0;
}

// -----------------------------------------------------------------------------
// Python part

static PyObject* py_to_lean(PyObject *self, PyObject *args) {
  const char *json;
  if (!PyArg_ParseTuple(args, "s", &json))
    return NULL;
  // TODO: raise python exception on error
  int res = parse(json);
  return PyLong_FromLong(res);
}

static PyMethodDef methods[] = {
  {"py_to_lean",  py_to_lean, METH_VARARGS, "Test python to lean"},
  {NULL, NULL, 0, NULL}
};

static struct PyModuleDef module = {
  PyModuleDef_HEAD_INIT, "lean_rffi", NULL, -1, methods
};

PyMODINIT_FUNC PyInit_lean_rffi(void) {
  if (lean_init())
    return NULL;
  return PyModule_Create(&module);
}

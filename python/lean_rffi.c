/*
Copyright (c) 2024 Amazon.com, Inc. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Paul Govereau
*/

// -----------------------------------------------------------------------------
// Lean part

#include <lean/lean.h>

extern void lean_initialize_runtime_module();
extern lean_object* initialize_NKL(uint8_t builtin, lean_object*);
extern lean_object* parse_json(lean_object*, lean_object*);

static lean_object *world = NULL;

int lean_init() {
  lean_initialize_runtime_module();
  world = lean_io_mk_world();
  lean_object *res = initialize_NKL(1, world);
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
  // lean_mk_string will copy the string
  lean_object *s = lean_mk_string(json);
  lean_object *res = parse_json(s, world);
  if (!lean_io_result_is_ok(res)) {
      // TODO: raise python exception rather than printing
      lean_io_result_show_error(res);
      lean_dec(res);
      return 1;
  }
  lean_dec_ref(res);
  return 0;
}

// -----------------------------------------------------------------------------
// Python part

#define PY_SSIZE_T_CLEAN
#include <Python.h>

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

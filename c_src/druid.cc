/********************************************************************
  Copyright (C) 2011 by Kelly L. McLaughlin

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*********************************************************************/

#include "erl_nif.h"
#include "uuid.h"

static ErlNifResourceType* druuid_RESOURCE;

// Prototypes
static ERL_NIF_TERM druuid_uuid_v4(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM error_tuple(ErlNifEnv* env,
                                ERL_NIF_TERM reason,
                                uuid_rc_t rc);

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_UUID_CREATE_FAILED;
static ERL_NIF_TERM ATOM_UUID_MAKE_FAILED;
static ERL_NIF_TERM ATOM_UUID_EXPORT_FAILED;
static ERL_NIF_TERM ATOM_UUID_DESTROY_FAILED;

static ErlNifFunc nif_funcs[] =
{
    {"v4", 0, druuid_uuid_v4}
};

static ERL_NIF_TERM druuid_uuid_v4(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    uuid_t *uuid;
    uuid_rc_t rc;
    void *vp = NULL;
    size_t n;

    if ((rc = uuid_create(&uuid)) != UUID_RC_OK)
    {
        return error_tuple(env, ATOM_UUID_CREATE_FAILED, rc);
    }

    if ((rc = uuid_make(uuid, UUID_MAKE_V4)) != UUID_RC_OK)
    {
        return error_tuple(env, ATOM_UUID_MAKE_FAILED, rc);
    }

    if ((rc = uuid_export(uuid, UUID_FMT_STR, &vp, &n)) != UUID_RC_OK)
    {
        return error_tuple(env, ATOM_UUID_EXPORT_FAILED, rc);
    }

    ERL_NIF_TERM value_bin;
    unsigned char* value = enif_make_new_binary(env, UUID_LEN_STR, &value_bin);
    memcpy(value, vp, UUID_LEN_STR);

    free(vp);

    if ((rc = uuid_destroy(uuid)) != UUID_RC_OK)
    {
        return error_tuple(env, ATOM_UUID_DESTROY_FAILED, rc);
    }

    return value_bin;
}

static void druuid_resource_cleanup(ErlNifEnv* env, void* arg)
{
}

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    druuid_RESOURCE = enif_open_resource_type(env, NULL,
                                                     "druuid_resource",
                                                     &druuid_resource_cleanup,
                                                     flags, NULL);
    if (druuid_RESOURCE == NULL)
    {
        return -1;
    }

    // Initialize common atoms
    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error")
    ATOM(ATOM_UUID_CREATE_FAILED, "uuid_create_failed");
    ATOM(ATOM_UUID_MAKE_FAILED, "uuid_make_failed");
    ATOM(ATOM_UUID_EXPORT_FAILED, "uuid_export_failed");
    ATOM(ATOM_UUID_DESTROY_FAILED, "uuid_destroy_failed");

    return 0;
}

ERL_NIF_TERM error_tuple(ErlNifEnv* env, ERL_NIF_TERM reason, uuid_rc_t rc)
{
    ERL_NIF_TERM return_code = enif_make_int(env, rc);
    return enif_make_tuple2(env, ATOM_ERROR,
                            enif_make_tuple2(env, reason, return_code));
}

ERL_NIF_INIT(druuid, nif_funcs, &on_load, NULL, NULL, NULL);

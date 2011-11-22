#include "erl_nif.h"

#include "uuid.h"
static ErlNifResourceType* druuid_RESOURCE;

// typedef struct
// {
// } druuid_handle;

// Prototypes
static ERL_NIF_TERM druuid_uuid(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"uuid", 1, druuid_uuid}
};

// static ERL_NIF_TERM druuid_new(ErlNifEnv* env, int argc,
//                                    const ERL_NIF_TERM argv[])
// {
//     druuid_handle* handle = enif_alloc_resource(druuid_RESOURCE,
//                                                     sizeof(druuid_handle));
//     ERL_NIF_TERM result = enif_make_resource(env, handle);
//     enif_release_resource(handle);
//     return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
// }


static ERL_NIF_TERM druuid_uuid(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void druuid_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in druuid_handle */
    /* druuid_handle* handle = (druuid_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    druuid_RESOURCE = enif_open_resource_type(env, NULL,
                                                     "druuid_resource",
                                                     &druuid_resource_cleanup,
                                                     flags, NULL);
    if (druuid_RESOURCE == NULL)
        return -1;

    return 0;
}

ERL_NIF_INIT(druuid, nif_funcs, &on_load, NULL, NULL, NULL);

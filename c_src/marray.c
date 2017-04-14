#include "inttypes.h"
#include "erl_nif.h"


ErlNifResourceType* RES_TYPE;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_bad_size;
ERL_NIF_TERM atom_alloc_error;


typedef struct marray{
    size_t size;
    uint16_t *array;
} marray;


static void destructor(ErlNifEnv* env, void *obj)
{
    marray *ptr = (marray *)obj;
    if(ptr->array)
        enif_free(ptr->array);
}


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_bad_size = enif_make_atom(env, "bad_size");
    atom_alloc_error = enif_make_atom(env, "alloc_error");
    int flags = ERL_NIF_RT_CREATE;
    RES_TYPE = enif_open_resource_type(env, "marray", "marray", destructor, flags, NULL);
    if (RES_TYPE == NULL) return -1;
    return 0;
}


static marray* get_array_pointer(ErlNifEnv* env, const ERL_NIF_TERM term)
{
    void *resource;
    enif_get_resource(env, term, RES_TYPE, &resource);
    return (marray *)resource;
}


// exported functions


static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    size_t size = 0;
    if (!enif_get_uint64(env, argv[0], &size)){
        ret = atom_bad_size;
        goto error;
    }
    marray *array = NULL;
    void *resource = enif_alloc_resource(RES_TYPE, sizeof(marray));
    if (!resource){
        ret = atom_alloc_error;
        goto error;
    }
    ret = enif_make_resource(env, resource);
    enif_release_resource(resource);
    array = (marray *)resource;
    array->size = size;
    array->array = enif_alloc(sizeof(uint16_t) * size);
    if (!array->array){
        ret = atom_alloc_error;
        goto error;
    }
    return enif_make_tuple2(env, atom_ok, ret);
error:
    return enif_make_tuple2(env, atom_error, ret);
}


static ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int pos;
    enif_get_uint(env, argv[0], &pos);
    marray* array = get_array_pointer(env, argv[1]);
    return enif_make_uint(env, array->array[pos]);
}


static ERL_NIF_TERM set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int pos;
    unsigned int value;
    marray* array = get_array_pointer(env, argv[2]);
    enif_get_uint(env, argv[0], &pos);
    enif_get_uint(env, argv[1], &value);
    array->array[pos] = value;
    return atom_ok;
}


static ERL_NIF_TERM count(ErlNifEnv *env, int agc, const ERL_NIF_TERM argv[])
{
    marray *array = get_array_pointer(env, argv[0]);
    return enif_make_uint(env, array->size);
}


static ErlNifFunc nif_funcs[] = {
    {"new", 1, new},
    {"get", 2, get},
    {"set", 3, set},
    {"count", 1, count}
};


ERL_NIF_INIT(marray_nif, nif_funcs, load, NULL, NULL, NULL)

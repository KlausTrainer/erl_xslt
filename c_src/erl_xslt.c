#include "erl_xslt.h"

/* This MUST be a power of two! */
#define HASHSIZE 1024
#if (!((HASHSIZE != 0) && !(HASHSIZE & (HASHSIZE - 1))))
#error
#endif

typedef struct {
    unsigned char* filename;
    xsltStylesheetPtr stylesheet;
} htab;

static unsigned int
hash(const unsigned char* filename)
{
	unsigned int hashval;

	for (hashval = 0; *filename != '\0'; filename++)
		hashval = (*filename + 31 * hashval) & (HASHSIZE - 1);
	return hashval;
}

static xsltStylesheetPtr
get(ErlNifEnv* env, const unsigned char* filename)
{
	htab** hashtab = enif_priv_data(env);
	htab* np = hashtab[hash(filename)];

	if (np && strcmp((char *) filename, (char *) np->filename) == 0) {
		return np->stylesheet;
	} else {
		return NULL;
	}
}

static int
put(ErlNifEnv* env, unsigned char* filename, xsltStylesheetPtr stylesheet)
{
	htab* np;

	ErlNifMutex* mutex = enif_mutex_create("erl_xslt");
	if (mutex == NULL)
		return 0;
	enif_mutex_lock(mutex);

	htab** hashtab = enif_priv_data(env);
	unsigned int hashval = hash(filename);
	xsltStylesheetPtr old_stylesheet = get(env, filename);

	if (old_stylesheet == NULL) {
		np = (htab*) enif_alloc(sizeof(*np));
		if (np == NULL)
			return 0;
		np->filename = filename;
		np->stylesheet = stylesheet;
		hashtab[hashval] = np;
	} else {
		enif_free(filename);
		xsltFreeStylesheet(old_stylesheet);
		hashtab[hashval]->stylesheet = stylesheet;
	}

	enif_mutex_unlock(mutex);
	enif_mutex_destroy(mutex);

	return 1;
}

static xsltStylesheetPtr
getStylesheet(ErlNifEnv* env, unsigned char* filename)
{
	xsltStylesheetPtr stylesheet = get(env, filename);

	if (!stylesheet) {
		stylesheet = xsltParseStylesheetFile(filename);
		put(env, filename, stylesheet);
	} else {
		enif_free(filename);
	}
	return stylesheet;
}

static unsigned char*
binary_to_string(ErlNifBinary* bin)
{
	unsigned char* str = (unsigned char*) enif_alloc(bin->size + 1);

	if (str == NULL)
		return NULL;
	strncpy((char *) str, (char *) bin->data, bin->size);
	str[bin->size] = '\0';
	return str;
}

static ERL_NIF_TERM
transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ERL_NIF_TERM term;
	ErlNifBinary arg0, arg1;

	unsigned char* xslt_filename;
	xsltStylesheetPtr xslt;

	unsigned char* input_xml_string;
	xmlDocPtr input_xml, xslt_result;
	xmlChar* doc_txt_ptr;
	int doc_txt_len;

	if (argc < 2
		|| !enif_inspect_binary(env, argv[0], &arg0)
		|| !enif_inspect_binary(env, argv[1], &arg1))
    		return enif_make_badarg(env);

	xslt_filename = binary_to_string(&arg0);
	input_xml_string = binary_to_string(&arg1);
	if (xslt_filename == NULL || input_xml_string == NULL)
		return enif_make_tuple2(
				env,
				enif_make_atom(env, "error"),
				enif_make_atom(env, "enomem"));

	xslt = getStylesheet(env, xslt_filename);
	if (!xslt) {
		enif_free(xslt_filename);
		enif_free(input_xml_string);
		return enif_make_tuple2(
				env,
				enif_make_atom(env, "error"),
				enif_make_atom(env, "no_stylesheet"));
	}

	input_xml = xmlParseDoc((const xmlChar *) input_xml_string);
	if (!input_xml)
		return enif_make_tuple2(
				env,
				enif_make_atom(env, "error"),
				enif_make_atom(env, "invalid_xml"));
	xslt_result = xsltApplyStylesheet(xslt, input_xml, NULL);
	if (!xslt_result)
		return enif_make_tuple2(
				env,
				enif_make_atom(env, "error"),
				enif_make_atom(env, "xslt_error"));

	xsltSaveResultToString(&doc_txt_ptr, &doc_txt_len, xslt_result, xslt);
	memcpy(
		enif_make_new_binary(env, doc_txt_len, &term),
		doc_txt_ptr,
		doc_txt_len);

	enif_free(input_xml_string);
	xmlFreeDoc(input_xml);
	xmlFreeDoc(xslt_result);
	xmlFree(doc_txt_ptr);

	return enif_make_tuple2(
			env,
			enif_make_atom(env, "ok"),
			term);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
	*priv = (htab*) enif_alloc(HASHSIZE * sizeof(htab*));
	if (*priv == NULL)
		return 1;
	bzero(*priv, HASHSIZE * sizeof(htab*));

	if (xmlMemSetup(enif_free,
			enif_alloc,
			enif_realloc,
			(xmlStrdupFunc) xmlStrdup) != 0)
		return 1;
	xmlInitParser();
	xmlSubstituteEntitiesDefault(1);

	return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
	*priv = *old_priv;
	return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
        xsltCleanupGlobals();
        xmlCleanupParser();
	enif_free(priv);
	return;
}


static ErlNifFunc funcs[] =
{
	{"transform", 2, transform}
};

ERL_NIF_INIT(erl_xslt, funcs, &load, NULL, &upgrade, &unload);

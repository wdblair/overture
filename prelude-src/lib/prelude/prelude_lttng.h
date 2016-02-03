/* Enable compilation in pedantic C99 mode */
#define typeof __typeof__
#define asm __asm__
#define BYTE_ORDER __BYTE_ORDER

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER prelude

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./prelude_lttng.h

#ifdef __cplusplus
#extern "C"{
#endif /*__cplusplus */


#if !defined(_PRELUDE_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _PRELUDE_LTTNG_H

#include <lttng/tracepoint.h>

/*
 * Trace points are defined here
 */

TRACEPOINT_EVENT(
    prelude,
    task, // Called when a task has executed
    TP_ARGS(const char *, id, int, cnt, const char *, ins, const char *, outs),
    TP_FIELDS(
        ctf_string(name, id)
        ctf_integer(int, instance, cnt)
        ctf_string(inputs, ins)
        ctf_string(outputs, outs)
    )
)

TRACEPOINT_EVENT(
    prelude,
    sensor, // Called when a sensor has executed
    TP_ARGS(const char *, id, int, cnt, const char *, outs),
    TP_FIELDS(
        ctf_string(name, id)
        ctf_integer(int, instance, cnt)
        ctf_string(outputs, outs)
    )
)

TRACEPOINT_EVENT(
    prelude,
    actuator, // Called when an actuator has executed
    TP_ARGS(const char *, id, int, cnt, const char *, ins),
    TP_FIELDS(
        ctf_string(name, id)
        ctf_integer(int, instance, cnt)
        ctf_string(inputs, ins)
    )
)

/*
 * Define log levels for trace points
 */

TRACEPOINT_LOGLEVEL(prelude, task, TRACE_INFO)

#endif /* _PRELUDE_LTTNG_H */

#include <lttng/tracepoint-event.h>

#ifdef __cplusplus
}
#endif /*__cplusplus */


# Prompt para el proyecto del dashboard

Copia `ww_trend.R` a la carpeta `R/` del proyecto Shiny (o donde estén las funciones) y pega el texto de abajo en la sesión de ese proyecto.

---

Quiero añadir al dashboard, para cada serie de aguas residuales (una planta y un patógeno), la curva suavizada de la concentración y la probabilidad de crecimiento en tiempo real. El cálculo ya está hecho y verificado en el archivo `R/ww_trend.R`, que acabo de copiar: **no reescribas ni modifiques las funciones de ese archivo**; solo llámalas. Lee primero su cabecera, que documenta el modelo, las reglas de datos y lo que devuelve cada función.

## Qué hay que construir

1. **Actualización de datos.** Cada vez que se refresquen los datos (al menos una vez por semana), para cada serie llama a
   `ww_realtime_update(published, date, conc, lod, delta)` con:
   - `date`, `conc`: todas las fechas y concentraciones de la serie (concentración original, sin log ni winsorizar; 0 = no detectado; NA o fecha ausente = sin muestra);
   - `lod`: el límite de detección del laboratorio si lo tenemos; si no, `NULL`;
   - `delta`: 0.21 para influenza A y RSV; 0.097 para SARS-CoV-2;
   - `published`: la tabla `$published` que devolvió la llamada anterior para esa serie (`NULL` la primera vez);
   - `prior`: para una serie con menos de dos años de datos (un patógeno o una planta que se empezó a monitorear hace poco) pasa `prior = WW_PRIOR`; así la serie empieza a publicar a los 60 días en lugar de a los dos años. Decide el valor una vez por serie y úsalo siempre igual en todas las llamadas de esa serie (con dos años de datos el a priori ya casi no influye, así que no hace falta quitarlo). Para series que ya tienen más de dos años se puede dejar `prior = NULL`.
   Si el dashboard ya tiene varias series establecidas (dos años o más) de otros patógenos o plantas, es mejor construir el a priori con ellas: ajusta cada una con `ww_trend()`, toma `attr(resultado, "params")` y pásalos a `ww_prior(sdB, noise)`; usa ese objeto en lugar de `WW_PRIOR`. Guárdalo en un archivo para no recalcularlo en cada actualización.
   Guarda `$published` de cada serie de forma persistente (por ejemplo un `.rds` por planta y patógeno, o una tabla en la base de datos del dashboard). **Esa tabla solo crece: nunca se recalcula ni se sobrescribe lo ya publicado.** Este paso debe ejecutarse en el proceso que actualiza los datos, no dentro de la sesión de cada usuario (la primera llamada de una serie tarda, porque reconstruye la historia).

2. **Gráfica de la serie** (panel superior): puntos con las concentraciones observadas en escala log10 (los no detectados con otro símbolo, en el límite de detección), la curva `$smooth$level_smooth` con su banda `smooth_lo`–`smooth_hi`, y los últimos días (`$smooth$provisional == TRUE`) con línea punteada o más tenue, con una nota: "los últimos 21 días de la curva son provisionales y pueden cambiar con datos nuevos".

3. **Gráfica de la probabilidad** (panel inferior, mismo eje de fechas): `$published$P_growth` como línea entre 0 y 1, una línea horizontal en 0.5, y los periodos con `$published$alert == TRUE` sombreados. Título sugerido: "Probabilidad de que la concentración haya crecido al menos un 62 % en 14 días" (25 % si `delta = 0.097`).

4. **Indicador actual** de la serie: la última `P_growth` publicada y si hay alerta activa. Según `$status`:
   - `"ok"`: mostrar el valor;
   - `"insufficient_history"`: mostrar "historia insuficiente" en lugar de un número (se necesitan 60 días con a priori, dos años sin él);
   - `"few_quantified"`: mostrar "muy pocas detecciones para estimar la tendencia";
   - `"no_new_data"`: mostrar lo ya publicado con la fecha del último dato.

## Reglas que debe respetar la integración

- La probabilidad y las alertas que se muestran salen **siempre de la tabla publicada**, no de un recálculo. Así lo pasado no cambia cuando llegan datos nuevos.
- La curva suavizada sí se recalcula en cada actualización (es retrospectiva por diseño); solo sus últimas semanas cambian de forma apreciable.
- No mostrar valores de nivel por debajo del límite de detección como si fueran una concentración: indicar "por debajo del límite de detección".
- En las series que usan a priori, mostrar una nota mientras tengan menos de unos seis meses de datos: "serie reciente: la probabilidad es menos precisa".
- Si hay que corregir datos históricos, decidir explícitamente si se regenera la tabla publicada de esa serie (borrándola y llamando de nuevo con `published = NULL`); no hacerlo de forma silenciosa.

Antes de escribir código, revisa cómo carga y guarda datos el dashboard hoy y propónme dónde encaja cada pieza (actualización, almacenamiento de `$published`, módulos de Shiny para las dos gráficas). Después de implementarlo, comprueba con una serie real que una segunda llamada con los mismos datos devuelve `status = "no_new_data"` y la misma tabla publicada.

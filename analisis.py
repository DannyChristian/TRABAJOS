import streamlit as st
import pandas as pd
import scipy.stats as stats
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

# Título de la app
st.title('Análisis Estadístico ENAHO 2022')

# Cargar archivo
st.sidebar.header('Cargar archivo CSV')
archivo = st.sidebar.file_uploader('Sube tu archivo CSV', type=['csv'])

if archivo is not None:
    df = pd.read_csv(archivo, encoding='latin1', sep=',')
    st.write('Datos cargados:')
    st.write(df.head())

    st.markdown('---')
    st.header('1️⃣ Chi-cuadrado: Asociación entre variables categóricas')

    var_cat1 = st.selectbox('Selecciona la primera variable categórica', df.columns)
    var_cat2 = st.selectbox('Selecciona la segunda variable categórica', df.columns)

    if var_cat1 and var_cat2:
        try:
            tabla_contingencia = pd.crosstab(df[var_cat1], df[var_cat2])
            chi2, p, dof, expected = stats.chi2_contingency(tabla_contingencia)

            st.write(f'**Chi-cuadrado:** {chi2:.4f}')
            st.write(f'**Valor p:** {p:.4f}')

            fig, ax = plt.subplots(figsize=(10, 6))
            tabla_contingencia.plot(kind='bar', ax=ax)
            plt.title(f'Distribución entre {var_cat1} y {var_cat2}')
            plt.ylabel('Frecuencia')
            plt.xticks(rotation=45)
            st.pyplot(fig)
        except Exception as e:
            st.write(f'Error en Chi-cuadrado: {e}')

    st.markdown('---')
    st.header('2️⃣ Comparación de medias con ANOVA (3 o más grupos)')

    var_grupo_a = st.selectbox('Selecciona la variable categórica (3 o más grupos)', df.columns, key='anova')
    var_numerica_a = st.selectbox('Selecciona la variable numérica', df.columns, key='anova_num')

    if var_grupo_a and var_numerica_a:
        grupos = df[var_grupo_a].dropna().unique()
        if len(grupos) >= 3:
            # Conversión segura a numérico
            df[var_numerica_a] = pd.to_numeric(df[var_numerica_a], errors='coerce')
            df_filtrado = df[[var_grupo_a, var_numerica_a]].dropna()

            if df_filtrado.empty:
                st.warning('Los datos filtrados no contienen información válida para el análisis.')
            else:
                filtrar_outliers = st.checkbox('Filtrar outliers extremos', key='anova_outliers')
                log_transform = st.checkbox('Aplicar transformación logarítmica', key='anova_log')

                if filtrar_outliers:
                    q_low = df_filtrado[var_numerica_a].quantile(0.01)
                    q_high = df_filtrado[var_numerica_a].quantile(0.99)
                    df_filtrado = df_filtrado[(df_filtrado[var_numerica_a] >= q_low) & (df_filtrado[var_numerica_a] <= q_high)]

                if log_transform:
                    df_filtrado[var_numerica_a] = np.log1p(df_filtrado[var_numerica_a])

                grupos_datos = [df_filtrado[df_filtrado[var_grupo_a] == grupo][var_numerica_a] for grupo in grupos]

                # Verificar que cada grupo tenga al menos dos datos distintos
                grupos_validos = [g for g in grupos_datos if len(g.dropna()) > 1 and np.var(g) > 0]

                if len(grupos_validos) >= 2:
                    f_stat, p_valor = stats.f_oneway(*grupos_validos)

                    st.write(f'**ANOVA F:** {f_stat:.4f}')
                    st.write(f'**Valor p:** {p_valor:.4f}')

                    fig, ax = plt.subplots(figsize=(10, 6))
                    sns.boxplot(x=var_grupo_a, y=var_numerica_a, data=df_filtrado, ax=ax)
                    plt.title(f'Boxplot de {var_numerica_a} por {var_grupo_a}')
                    plt.xticks(rotation=45)
                    st.pyplot(fig)

                    st.write('Distribución de los datos por grupo:')
                    fig, ax = plt.subplots()
                    for grupo in grupos:
                        subset = df_filtrado[df_filtrado[var_grupo_a] == grupo]
                        sns.histplot(subset[var_numerica_a], kde=True, label=str(grupo), ax=ax)
                    plt.legend()
                    st.pyplot(fig)
                else:
                    st.warning('Los grupos seleccionados no tienen suficiente variabilidad para realizar ANOVA. Por favor, selecciona otras variables.')
        else:
            st.warning('La variable seleccionada debe tener 3 o más grupos diferentes.')

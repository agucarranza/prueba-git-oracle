CREATE OR REPLACE PACKAGE PKG_Pruebas AS
  TYPE ty_CURSOR IS REF CURSOR;
  v_ERROR exception;
  V_NO_HAY_AS_TIPO exception;
  PROCEDURE SUGIERO_PROX_CODIGO(P_ID_CTA_CONTABLE IN SUAF.T_CUENTAS_CONTABLES.ID_CTA_CONTABLE%TYPE DEFAULT NULL,
                                P_ID_VIGENCIA     IN SGP.T_VIGENCIAS.ID_VIGENCIA%TYPE DEFAULT NULL,
                                COD_SIG_NVO       OUT VARCHAR2,
                                P_MSG             OUT VARCHAR2);

/*  FUNCTION PROX_NUM_DISPONIBLE(P_ID_CTA_CONTABLE IN SUAF.T_CUENTAS_CONTABLES.ID_CTA_CONTABLE%TYPE DEFAULT NULL,
                               v_id_jurisd       in sgp.t_jurisdicciones.id_jurisdiccion%TYPE DEFAULT NULL)
    return varchar2;*/

  PROCEDURE BUSCAR_DEV_PORIDRENDICION(p_T_RENDICION         OUT ty_CURSOR,
                                      p_ID_VIGENCIA         IN NUMBER DEFAULT NULL,
                                      p_ID_UNIDAD_ADM       IN SUAF.T_RENDICION.ID_UNIDAD_ADM%TYPE DEFAULT NULL,
                                      p_ID_RENDICION        IN SUAF.T_ANTICIPOS.ID_RENDICION%TYPE DEFAULT NULL,
                                      p_PAGINA              IN NUMBER DEFAULT NULL,
                                      p_TAMANOPAGINA        IN NUMBER DEFAULT NULL,
                                      p_CANTIDADDEREGISTROS OUT NUMBER);
/*PROCEDURE BUSCAR_ASIENTO_TIPO_REL
(P_T_ASIENTO_TIPO_REL     OUT Ty_CURSOR,
 P_T_ASIENTO_TIPO_DET_REL OUT Ty_CURSOR,
 P_ID_OBJETO_GASTO        IN SGP.T_OBJETOS_GASTO.ID_OBJETO_GASTO%TYPE DEFAULT NULL,
 P_ID_VIGENCIA            IN SGP.T_VIGENCIAS.ID_VIGENCIA%TYPE DEFAULT NULL,
 p_MSG                    OUT VARCHAR2
);*/

PROCEDURE TIPO_ETAPA_RUR
(
   p_T_ETAPA OUT ty_CURSOR,
   p_id_tipodoc IN suaf.t_etapas.id_etapa%TYPE DEFAULT NULL

);




PROCEDURE PRC_03_TC
  -- Autor: Ma. Laura Martínez
    -- Fecha: 15/03/2010
    -- Modificado: 29/03/2010 E.Juarez
  
  (p_T_CURSOR      OUT ty_CURSOR,
   p_ID_VIGENCIA   IN SGP.T_VIGENCIAS.ID_VIGENCIA%TYPE DEFAULT NULL,
   P_ID_FONDO      IN SUAF.T_FONDO_CAJACH.ID_FONDO_CAJA%TYPE DEFAULT NULL,
   p_NRO_RENDICION IN SUAF.T_RENDICION.ID_RENDICION%TYPE DEFAULT NULL -- toma el id_rendicion
   );

end PKG_Pruebas;

 
/
CREATE OR REPLACE PACKAGE BODY PKG_Pruebas AS

-- este es un comentario de prueba 
  
PROCEDURE SUGIERO_PROX_CODIGO

( P_ID_CTA_CONTABLE IN SUAF.T_CUENTAS_CONTABLES.ID_CTA_CONTABLE%TYPE DEFAULT NULL,
  P_ID_VIGENCIA IN SGP.T_VIGENCIAS.ID_VIGENCIA%TYPE DEFAULT NULL,
  COD_SIG_NVO OUT VARCHAR2,
  P_MSG OUT VARCHAR2
)
IS

v_long_cod_padre varchar2(10);
v_cod_exis_may varchar2(10);
v_cod_padre varchar2(10);
v_id_jurisd varchar2(10);
V_INDICE number;

BEGIN

  /* select Distinct(cc.id_jurisdiccion)
   into v_id_jurisd
   from suaf.t_cuentas_contables cc , sgp.t_jurisdicciones j
   where cc.id_jurisdiccion = j.id_jurisdiccion
   and j.id_vigencia = P_ID_VIGENCIA
   and j.numero_jurisdiccion = 0;    */

    /*   --Si se trata de un padre
       if P_ID_CTA_CONTABLE is null then

          Select max(cc.codigo)
          into v_cod_exis_may
          from suaf.t_cuentas_contables cc
          where cc.id_cta_contable_rel is null
          and cc.id_jurisdiccion = v_id_jurisd  ;
        
       --Si se trata de un hijo
       else
          select cc.codigo
          into v_cod_padre
          from suaf.t_cuentas_contables cc
          where cc.id_jurisdiccion = v_id_jurisd  
          and cc.id_cta_contable=P_ID_CTA_CONTABLE;

          v_long_cod_padre := (length(v_cod_padre));

          select MAX(substr(c.codigo, v_long_cod_padre + 1))
          into v_cod_exis_may
          from t_cuentas_contables c
          where c.id_jurisdiccion =    v_id_jurisd
          and length(c.codigo)=v_long_cod_padre+2
          and c.codigo like ''||v_cod_padre||'%';

      end if;      
    */
 --por si es el primer --
      if v_cod_exis_may is null then
        v_cod_exis_may :=0;
      end if;
    
 --Cuando el Maximo llega a 99 buscamos el primero disponible de padres
/*    
   if v_cod_exis_may = 99 then      
 
     \*  SELECT pkg_pruebas.PROX_NUM_DISPONIBLE(P_ID_CTA_CONTABLE,v_id_jurisd)
       into COD_SIG_NVO
       FROM DUAL;
      *\
   else            
          COD_SIG_NVO := v_cod_exis_may + 1;          
   end if;    */   
       
      if length(COD_SIG_NVO)=1 then
          COD_SIG_NVO := 0||COD_SIG_NVO;
      end if;
 
  
    EXCEPTION             
      WHEN OTHERS THEN
      p_msg := 'Error en PKG_CUENTAS_CONTABLES.SUGIERO_PROX_CODIGO en T_CUENTAS_CONTABLES Verifique: ' ||sqlerrm;

END SUGIERO_PROX_CODIGO; 

/*FUNCTION PROX_NUM_DISPONIBLE
(
 P_ID_CTA_CONTABLE IN SUAF.T_CUENTAS_CONTABLES.ID_CTA_CONTABLE%TYPE DEFAULT NULL,
 v_id_jurisd in sgp.t_jurisdicciones.id_jurisdiccion%TYPE DEFAULT NULL
)
return varchar2

IS 
v_cod_exis_may varchar2(10);
V_INDICE number;
v_cod_padre varchar2(10);
v_long_cod_padre varchar2(10);
BEGIN       

        if P_ID_CTA_CONTABLE is null then
   
          FOR i IN 1..99 LOOP
                v_cod_exis_may:=i;
                if length(v_cod_exis_may)=1 then
                   v_cod_exis_may := 0||v_cod_exis_may;
                end if;       
                
                SELECT i 
                INTO V_INDICE 
                FROM DUAL
                WHERE i  IN  ( -- LISTA DE NUMERO USADOS PARA PADRES           
                               select substr(c.codigo,0,2)
                               from suaf.t_cuentas_contables c
                               where c.id_jurisdiccion = v_id_jurisd
                               and length(c.codigo)=2                 
                              );                              
                end LOOP;
     
      else --si es un hijo
      
        select cc.codigo
          into v_cod_padre
          from suaf.t_cuentas_contables cc
          where cc.id_jurisdiccion = v_id_jurisd  
          and cc.id_cta_contable=P_ID_CTA_CONTABLE;

          v_long_cod_padre := (length(v_cod_padre));
         
                    FOR i IN 1..99 LOOP
                v_cod_exis_may:=i;
                if length(v_cod_exis_may)=1 then
                   v_cod_exis_may := 0||v_cod_exis_may;
                end if;       
                
                SELECT i 
                INTO V_INDICE 
                FROM DUAL
                WHERE i  IN  (  select substr(c.codigo, v_long_cod_padre + 1)
                                --into v_cod_exis_may
                                from t_cuentas_contables c
                                where c.id_jurisdiccion =    v_id_jurisd
                                and length(c.codigo)=v_long_cod_padre+2
                                and c.codigo like ''||v_cod_padre||'%'
                     
                              );                              
                end LOOP;
   
       end if;

 exception 
    WHEN no_data_found THEN 
       return v_cod_exis_may;  
       
end PROX_NUM_DISPONIBLE;
*/
PROCEDURE BUSCAR_DEV_PORIDRENDICION
    /*
    * Version 0
    * Ahora solo lee tabla T_DEVENGADO, y busca id_anticipo de la misma.
    */
(
    p_T_RENDICION OUT ty_CURSOR,
    p_ID_VIGENCIA IN NUMBER DEFAULT NULL,
    p_ID_UNIDAD_ADM IN SUAF.T_RENDICION.ID_UNIDAD_ADM%TYPE DEFAULT NULL,
    p_ID_RENDICION IN SUAF.T_ANTICIPOS.ID_RENDICION%TYPE DEFAULT NULL,
    p_PAGINA IN NUMBER DEFAULT NULL,
    p_TAMANOPAGINA IN NUMBER DEFAULT NULL,
    p_CANTIDADDEREGISTROS OUT NUMBER
)
IS
BEGIN
    OPEN p_T_RENDICION FOR
    
        SELECT 
            D.ID_DEVENGADO ID,
            
            ROWNUM CANT, 
            
            'EGRESO' MOVIMIENTO,
            
            (SELECT
                 P.NOMBRE
             FROM 
                 TESORERIA.T_PROVEEDORES P
             WHERE
                 P.ID_PROVEEDOR = D.ID_PROVEEDOR
            ) PROVEEDOR,
            
            (SELECT 
                 TO_CHAR(C.ID_BENEFICIARIO)
             FROM 
                 TESORERIA.T_COMPROBANTES C 
             WHERE 
                 C.ID_DEVENGADO = D.ID_DEVENGADO) NRO_MOVIMIENTO,
 
                 
            (SELECT 
                 CB.NRO_CUENTA 
             FROM 
                 TESORERIA.T_CUENTAS_BANCARIA CB 
             WHERE 
                 CB.ID_CUENTA_BANCARIA = D.ID_CUENTA_BANCARIA) NRO_CUENTA,
                 
            (SELECT 
                 SUM(DD.IMPORTE*DD.CANTIDAD) 
             FROM 
                 SUAF.T_DEVENGADO_DETALLE DD 
             WHERE 
                 DD.ID_DEVENGADO = D.ID_DEVENGADO) IMPORTE,
                 
            (SELECT DECODE((SELECT COUNT(*)
                           FROM 
                               TESORERIA.T_COMPROBANTES C,
                               TESORERIA.T_EP_LIQ_DET_COMP LDC,
                               TESORERIA.T_EP_LIQUIDACION_DETALLE LD,
                               TESORERIA.T_ELEMENTOS_PAGO EP
                           WHERE
                               C.ID_DEVENGADO = D.ID_DEVENGADO
                           AND     
                               C.ID_COMPROBANTE = LDC.ID_COMPROBANTE
                           AND     
                               LDC.ID_EP_LIQUIDACION_DETALLE = LD.ID_EP_LIQUIDACION_DETALLE
                           AND     
                               LD.ID_ELEMENTO_PAGO = EP.ID_ELEMENTO_PAGO),0,'NO TIENE'
                                                                         ,1,(SELECT
                                                                                 EP.NUMERO_EP   
                                                                              FROM 
                                                                                  TESORERIA.T_COMPROBANTES C,
                                                                                  TESORERIA.T_EP_LIQ_DET_COMP LDC,
                                                                                  TESORERIA.T_EP_LIQUIDACION_DETALLE LD,
                                                                                  TESORERIA.T_ELEMENTOS_PAGO EP
                                                                              WHERE
                                                                                  C.ID_DEVENGADO = D.ID_DEVENGADO
                                                                              AND     
                                                                                  C.ID_COMPROBANTE = LDC.ID_COMPROBANTE
                                                                              AND     
                                                                                  LDC.ID_EP_LIQUIDACION_DETALLE = LD.ID_EP_LIQUIDACION_DETALLE
                                                                              AND     
                                                                                  LD.ID_ELEMENTO_PAGO = EP.ID_ELEMENTO_PAGO)       
                                                                         ,'VARIOS') CHEQUE_TRANS
            FROM DUAL) CHEQUE_TRANS, 
                 
            (SELECT DECODE((SELECT
                                 COUNT(*)
                             FROM 
                                 TESORERIA.T_COMPROBANTES C,
                                 TESORERIA.T_EP_LIQ_DET_COMP LDC,
                                 TESORERIA.T_EP_LIQUIDACION_DETALLE LD,
                                 TESORERIA.T_ELEMENTOS_PAGO EP
                             WHERE
                                 C.ID_DEVENGADO = D.ID_DEVENGADO
                             AND     
                                 C.ID_COMPROBANTE = LDC.ID_COMPROBANTE
                             AND     
                                 LDC.ID_EP_LIQUIDACION_DETALLE = LD.ID_EP_LIQUIDACION_DETALLE
                             AND     
                                 LD.ID_ELEMENTO_PAGO = EP.ID_ELEMENTO_PAGO),0,null--'NO TIENE'
                                                                         ,1,(SELECT
                                                                                   EP.FECHA_PAGADO   
                                                                               FROM 
                                                                                   TESORERIA.T_COMPROBANTES C,
                                                                                   TESORERIA.T_EP_LIQ_DET_COMP LDC,
                                                                                   TESORERIA.T_EP_LIQUIDACION_DETALLE LD,
                                                                                   TESORERIA.T_ELEMENTOS_PAGO EP
                                                                               WHERE
                                                                                   C.ID_DEVENGADO = D.ID_DEVENGADO
                                                                               AND     
                                                                                   C.ID_COMPROBANTE = LDC.ID_COMPROBANTE
                                                                               AND     
                                                                                   LDC.ID_EP_LIQUIDACION_DETALLE = LD.ID_EP_LIQUIDACION_DETALLE
                                                                               AND     
                                                                                   LD.ID_ELEMENTO_PAGO = EP.ID_ELEMENTO_PAGO)        
                                                                         ,null/*'VARIOS'*/) FECHA_PAGO 
             FROM DUAL) FECHA_PAGO 
                 
        FROM 
            SUAF.T_DEVENGADO D
        WHERE
            D.NRO_RENDICION = p_ID_RENDICION
        --AND
        --    D.ID_ANTICIPO IS NULL -- ANTICIPOS EXCLUYENTE
            
        -- <-- T_DEVENGADO    
        
/*        UNION    
        
        -- --> T_DEVENGADO - ID_ANTICIPO
        
        SELECT 
            D.ID_DEVENGADO ID,
            ROWNUM CANT, 
            'ANTICIPO' MOVIMIENTO,
            (SELECT
                 P.NOMBRE
             FROM 
                 TESORERIA.T_PROVEEDORES P
             WHERE
                 P.ID_PROVEEDOR = D.ID_PROVEEDOR
            ) PROVEEDOR,
            (SELECT 
                 TO_CHAR(C.ID_BENEFICIARIO)
             FROM 
                 TESORERIA.T_COMPROBANTES C 
             WHERE 
                 C.ID_DEVENGADO = D.ID_DEVENGADO) NRO_MOVIMIENTO,
            (SELECT 
                 CB.NRO_CUENTA 
             FROM 
                 TESORERIA.T_CUENTAS_BANCARIA CB 
             WHERE 
                 CB.ID_CUENTA_BANCARIA = D.ID_CUENTA_BANCARIA) NRO_CUENTA,
            (SELECT 
                 SUM(DD.IMPORTE*DD.CANTIDAD) 
             FROM 
                 SUAF.T_DEVENGADO_DETALLE DD 
             WHERE 
                 DD.ID_DEVENGADO = D.ID_DEVENGADO) IMPORTE,
            (SELECT
                 EP.NUMERO_EP   
             FROM 
                 TESORERIA.T_COMPROBANTES C,
                 TESORERIA.T_EP_LIQ_DET_COMP LDC,
                 TESORERIA.T_EP_LIQUIDACION_DETALLE LD,
                 TESORERIA.T_ELEMENTOS_PAGO EP
             WHERE
                 C.ID_DEVENGADO = D.ID_DEVENGADO
             AND     
                 C.ID_COMPROBANTE = LDC.ID_COMPROBANTE
             AND     
                 LDC.ID_EP_LIQUIDACION_DETALLE = LD.ID_EP_LIQUIDACION_DETALLE
             AND     
                 LD.ID_ELEMENTO_PAGO = EP.ID_ELEMENTO_PAGO) CHEQUE_TRANS, 
            (SELECT
                 EP.FECHA_PAGADO   
             FROM 
                 TESORERIA.T_COMPROBANTES C,
                 TESORERIA.T_EP_LIQ_DET_COMP LDC,
                 TESORERIA.T_EP_LIQUIDACION_DETALLE LD,
                 TESORERIA.T_ELEMENTOS_PAGO EP
             WHERE
                 C.ID_DEVENGADO = D.ID_DEVENGADO
             AND     
                 C.ID_COMPROBANTE = LDC.ID_COMPROBANTE
             AND     
                 LDC.ID_EP_LIQUIDACION_DETALLE = LD.ID_EP_LIQUIDACION_DETALLE
             AND     
                 LD.ID_ELEMENTO_PAGO = EP.ID_ELEMENTO_PAGO) FECHA_PAGO
        FROM 
            SUAF.T_DEVENGADO D
        WHERE
            D.ID_ANTICIPO IS NOT NULL
        AND
            D.NRO_RENDICION = p_ID_RENDICION*/
    ;
    
    p_CANTIDADDEREGISTROS := 1;    
     
END BUSCAR_DEV_PORIDRENDICION;

/*PROCEDURE BUSCAR_ASIENTO_TIPO_REL
(
   P_T_ASIENTO_TIPO_REL OUT Ty_CURSOR,
   P_T_ASIENTO_TIPO_DET_REL OUT Ty_CURSOR,
   P_ID_OBJETO_GASTO IN SGP.T_OBJETOS_GASTO.ID_OBJETO_GASTO%TYPE DEFAULT NULL,
   P_ID_VIGENCIA IN SGP.T_VIGENCIAS.ID_VIGENCIA%TYPE DEFAULT NULL,
   p_MSG OUT VARCHAR2     
)
IS
V_ID_ASIENTO_TIPO NUMBER;
   
  CURSOR LAURA IS
  
     SELECT distinct(MC.ID_ASIENTO_TIPO) id_asiento_tipo
   INTO V_ID_ASIENTO_TIPO
   FROM SUAF.T_MATRIZ_CONTABLE MC
   WHERE MC.ID_VIGENCIA = P_ID_VIGENCIA
   AND MC.ID_CLASIF_PPAL = P_ID_OBJETO_GASTO
   AND MC.N_CLASIF_PPAL LIKE 'OG';

   

 BEGIN

       \*if V_ID_ASIENTO_TIPO is not null then*\
    
\*    for r_asiento in LAURA loop
    
     V_ID_ASIENTO_TIPO := LAURA.ID_ASIENTO_TIPO;
  
  end loop;*\
  
 \* END IF;*\
  --END;
    SELECT *
    INTO V_ID_ASIENTO_TIPO
   FROM DUAL;


IS 
 V_ID_ASIENTO_TIPO NUMBER;
 V_CANTIDAD_AS_TIPO NUMBER;
 error number;
 
 begin
 cursor asiento is

  
   SELECT distinct(MC.ID_ASIENTO_TIPO) id_asiento_tipo
   INTO V_ID_ASIENTO_TIPO
   FROM SUAF.T_MATRIZ_CONTABLE MC
   WHERE MC.ID_VIGENCIA = P_ID_VIGENCIA
   AND MC.ID_CLASIF_PPAL = P_ID_OBJETO_GASTO
   AND MC.N_CLASIF_PPAL LIKE 'OG';
 
 BEGIN 

    if V_ID_ASIENTO_TIPO is not null then
    
    for r_asiento in asiento loop
    
     V_ID_ASIENTO_TIPO := asiento.id_asiento_tipo;
  
  end loop;
    
  \*   open P_T_ASIENTO_TIPO_DET_REL for
       SELECT *     
       FROM SUAF.T_ASIENTOS_TIPO_DET ATD
       WHERE ATD.ID_ASIENTO_TIPO = V_ID_ASIENTO_TIPO;*\
       
\*   
  else 
  
       error := 1;*\
  
      end if;
end;

 \*open P_T_ASIENTO_TIPO_REL for
  
   SELECT distinct(MC.ID_ASIENTO_TIPO)
   INTO V_ID_ASIENTO_TIPO
   FROM SUAF.T_MATRIZ_CONTABLE MC
   WHERE MC.ID_VIGENCIA = P_ID_VIGENCIA
   AND MC.ID_CLASIF_PPAL = P_ID_OBJETO_GASTO
   AND MC.N_CLASIF_PPAL LIKE 'OG';
   --AND MC.ID_ASIENTO_TIPO IS NOT NULL;
   
   open P_T_ASIENTO_TIPO_DET_REL for
       SELECT *     
       FROM SUAF.T_ASIENTOS_TIPO_DET ATD
       WHERE ATD.ID_ASIENTO_TIPO = V_ID_ASIENTO_TIPO;  *\
\*
  LOOP
      FETCH P_T_ASIENTO_TIPO_REL
        INTO v_ID_ASIENTO_TIPO;
      EXIT WHEN P_T_ASIENTO_TIPO_REL%NOTFOUND;
    END LOOP;*\
   \*
   --RECORRO EL CURSOR QUE TIENE LOS ID_ASIENTO_TIPO Y BUSCO LOS DETALLES
      FOR r_P_T_ASIENTO_TIPO_REL IN P_T_ASIENTO_TIPO_REL LOOP 
        
      OPEN P_T_ASIENTO_TIPO_DET_REL FOR
      
       SELECT *
       INTO V_ID_ASIENTO_TIPO
       FROM SUAF.T_ASIENTOS_TIPO_DET ATD
       WHERE ATD.ID_ASIENTO_TIPO = r_P_T_ASIENTO_TIPO_REL;  
   
      END LOOP;*\
   
                 \* exit when valor = 1;*\
   
   
\* exception
    WHEN no_data_found THEN
      p_MSG := 'No existen Asientos Tipos relacionados a la Cuenta Contable por lo que no hay entradas a la Matriz Contable.';
    when others then
      p_MSG := 'Error al Buscar Asientos Tipos por Cuentas Contables Verifique: ' ||
               sqlerrm;*\

 

END BUSCAR_ASIENTO_TIPO_REL;*/

PROCEDURE TIPO_ETAPA_RUR
(
    p_T_ETAPA OUT ty_CURSOR,
    p_id_tipodoc IN suaf.t_etapas.id_etapa%TYPE DEFAULT NULL
)
IS
BEGIN

    OPEN p_T_ETAPA FOR
      select distinct(t.id_etapa) ID_ETAPA, t.descripcion NOMBRE_TIPO_ETAPA
      from t_etapas t, t_tipodoc_etapas tde, t_tipodoc td
      where t.id_etapa = tde.id_etapa
      and tde.id_tipodoc = td.id_tipodoc
      and td.id_tipodoc = p_id_tipodoc;                        
                                    
END TIPO_ETAPA_RUR;


 
 PROCEDURE PRC_03_TC
  -- Autor: Ma. Laura Martínez
    -- Fecha: 15/03/2010
    -- Modificado: 29/03/2010 E.Juarez
  
  (p_T_CURSOR      OUT ty_CURSOR,
   p_ID_VIGENCIA   IN SGP.T_VIGENCIAS.ID_VIGENCIA%TYPE DEFAULT NULL,
   P_ID_FONDO      IN SUAF.T_FONDO_CAJACH.ID_FONDO_CAJA%TYPE DEFAULT NULL,
   p_NRO_RENDICION IN SUAF.T_RENDICION.ID_RENDICION%TYPE DEFAULT NULL -- toma el id_rendicion
   ) IS
    p_nro_actual       numeric;
    p_nro_anterior_rp  numeric;
    p_fecha_cierre_rp  DATE; --varchar2(10) ;
    p_fecha_cierre_ant DATE; -- varchar2(10);
    p_fecha_desde      DATE; -- varchar2(10);
    p_fecha_hasta      DATE; -- varchar2(10);
  
    V_SENTENCIA    varchar2(100);
    v_bandera      number := 0;
    v_fec_rend_ant varchar2(10);
    v_cant_reg     number;
  
    v_letra_cod           varchar2(1000);
    v_denominacion        varchar2(1000);
    v_numero_unidad_adm   varchar2(1000);
    v_nombre_unidad_adm   varchar2(1000);
    v_ejercicio           varchar2(1000);
    v_numero_jurisdiccion varchar2(1000);
    v_nombre_jurisdiccion varchar2(1000);
    v_monto_fondo         number(18, 2);
  
    --agregado por Lau Martínez-------------
    v_nombre_fte_finan varchar2(1000);
    ----------------------------------------
    --agregado por Richard -------------
    v_Resoluciones_fdo varchar2(1000);
    ----------------------------------------
  
    v_ID_VIGENCIA1   SGP.T_VIGENCIAS.ID_VIGENCIA%TYPE DEFAULT NULL;
    v_ID_FONDO1      SUAF.T_FONDO_CAJACH.ID_FONDO_CAJA%TYPE DEFAULT NULL;
    v_NRO_RENDICION1 SUAF.T_RENDICION.ID_RENDICION%TYPE DEFAULT NULL;
  
  BEGIN
  
    v_ID_VIGENCIA1   := p_ID_VIGENCIA;
    v_ID_FONDO1      := P_ID_FONDO;
    v_NRO_RENDICION1 := p_NRO_RENDICION;
  
    V_SENTENCIA := '  ALTER SESSION SET NLS_DATE_FORMAT=' || '''' ||
                   'DD/MM/YYYY HH:MI:SS' || ''''; -- modificado por E.J 2010-04-22 para que tenga en cuenta hh:mm_ss
    --dd/mm/yyyy hh24:mi:ss
  
    EXECUTE IMMEDIATE V_SENTENCIA;
  
    select r.nro_rendicion, r.fec_cierre --to_char(r.fec_cierre, 'dd/mm/yyyy') -- modificado por E.J 2010-04-22 para que tenga en cuenta hh:mm_ss
      into p_nro_actual, p_fecha_cierre_rp
      from suaf.t_rendicion r
     where r.id_fondo_caja = v_ID_FONDO1
       and r.id_rendicion = v_NRO_RENDICION1; --p_id_RENDICION;
  
    -- busco parametros para pasarle a la consulta si no deveulve datos
  
    select v.ejercicio
      into v_ejercicio
      from sgp.t_vigencias v
     where v.id_vigencia = v_ID_VIGENCIA1;
  
    IF p_nro_actual = 1 then
      --Es la primer Rendicion
      --      p_fecha_desde := '01/01/2010 01:00:00'; -- modificado por E.J 2010-04-22 para que tenga en cuenta hh:mm_ss
      p_fecha_desde := '01/01/' || v_ejercicio || ' 01:00:00';
    
      p_fecha_hasta := p_fecha_cierre_rp;
    
    else
      --No es la primer rendición
      select r.fec_cierre --TRUNC(r.fec_cierre) -- modificado por E.J 2010-04-22 para que tenga en cuenta hh:mm_ss
        into p_fecha_cierre_ant
        from suaf.t_rendicion r
       where r.id_fondo_caja = v_ID_FONDO1
         and r.nro_rendicion = (p_nro_actual - 1)
         and r.id_estado_rcch <> 16;
    
      p_fecha_desde :=  p_fecha_cierre_ant; --v_fec_rend_ant;
     p_fecha_hasta :=  p_fecha_cierre_rp;
    
    end if;
  
    --MODIFICADO POR LAURA MARTINEZ 27/04/2010 11:10 HS
    --SE QGREGO LA FUENTE DE FINANCIAMIENTO PARA Q SEA DEVUELTA EN EL CONSULTA
  
  select fcc.letra_cod,
         fcc.denominacion,
         --agregado por Richard ---------
         suaf.pkg_reportes_aux.F_Resoluciones(fcc.id_fondo_caja),
         --------------------------
         ua.numero_unidad_adm,
         ua.nombre_unidad_adm,
         vgn.ejercicio,
         j.numero_jurisdiccion,
         j.nombre_jurisdiccion,
         --agregado por LM---------
         ff.nombre_fuente_fin, 
        
         --------------------------
         fcc.monto monto/* ,+ (select nvl(sum(monto), 0)
                        from suaf.t_resoluciones r
                       where r.id_fondo_caja = v_ID_FONDO1
                           -- and r.fec_emision <= trunc(p_fecha_desde)
                              
                            -- between '01/01/2010' and '31/12/2017'
                        -- and TO_DATE(r.fec_emision, 'DD/MM/YYYY') <= TO_DATE(trunc(p_fecha_hasta), 'DD/MM/YYYY')
                           
                        
                      ) monto*/
    
    into v_letra_cod,
         v_denominacion,
         v_Resoluciones_fdo,
         v_numero_unidad_adm,
         v_nombre_unidad_adm,
         v_ejercicio,
         v_numero_jurisdiccion,
         v_nombre_jurisdiccion,
         v_nombre_fte_finan
         ,v_monto_fondo
    
    from suaf.t_fondo_cajach           fcc,
         sgp.t_unidades_administrativa ua,
         sgp.t_vigencias               vgn,
         sgp.t_jurisdicciones          j,
         sgp.t_fuentes_financiamiento  ff
    
   where fcc.id_unidad_adm = ua.id_unidad_adm
     and ua.id_vigencia = vgn.id_vigencia
     and ua.id_jurisdiccion = j.id_jurisdiccion
        --agregado pro lau martinez --------------------
     and fcc.id_fuente_fin = ff.id_fuente_fin
        ------------------------------------------------
     and id_fondo_caja = v_ID_FONDO1;
  
    -- si el count en 0 no devuelve registros y se fuerza a que devuelva al menos 1
  
    --CONSULTA  MODIFICADA POR E.JUAREZ 29/03/2010 NO MODIFICAR !!!!!!
  
    SELECT count(*)
      into v_cant_reg
      from suaf.vt_oefondos_03tc_cab vt
     where ID_VIGENCIA = v_ID_VIGENCIA1
       AND ID_FONDO_CAJA = v_ID_FONDO1
          --    AND vt.nId_Rendicion = v_NRO_RENDICION11 --p_id_RENDICION
          --       AND trunc(FEC_ALTA) between p_fecha_desde and p_fecha_hasta;
          --       AND FEC_ALTA between p_fecha_desde and p_fecha_hasta;
       AND FEC_ALTA > p_fecha_desde
       and FEC_ALTA <= p_fecha_hasta; -- modificado por E.J 2012-02-28 para que tome las OE con fecha mayor que la fecha de cierre anterior y menor igual que la actual.
  
    if v_cant_reg <> 0 then
    
      OPEN p_T_CURSOR FOR
      
      -- ingresos de ordenes de entrega
        select numero_unidad_adm,
               unidad_adm,
               numero_jurisdiccion,
               jurisd,
               ---agregado por LM---------
               NOMBRE_FUENTE_FIN,
               ---------------------------
               round(año) año,
               p_fecha_cierre_rp FECHA_CIERRE, --fecha_cierre,
               p_nro_actual nro_rendicion, --nro_rendicion,
               id_fondo_caja,
               n_fondo,
               letra_cod_f,
               suaf.pkg_reportes_aux.F_Resoluciones(id_fondo_caja) resoluciones, --- Richard
               -- se agrego el 07/06/2011 Gali 9681 E.J
               monto_fondo + (select nvl(sum(monto), 0)
                                from suaf.t_resoluciones r
                               where id_fondo_caja = v_ID_FONDO1
                                    --and r.fec_emision between p_fecha_desde and p_fecha_hasta
                                -- and r.fec_emision <= p_fecha_hasta /*and r.fec_emision <= p_fecha_hasta */
                              ) monto_fondo,
               id_oe,
               nro_oe,
               fec_alta FECHA_MOV_DIA,
               tipomovimiento,
               nro_cta_ban,
               denomi_cb,
               importe_oe,
               id_vigencia,
               (select nvl(sum(vt.IMPORTE_OE), 0)
                   from vt_oefondos_03tc_CAB vt
                  where ID_VIGENCIA = v_ID_VIGENCIA1
                    AND ID_FONDO_CAJA = v_ID_FONDO1
                       --                   AND trunc(FEC_ALTA) between p_fecha_desde and  p_fecha_hasta
                       --   AND FEC_ALTA between p_fecha_desde and p_fecha_hasta -- modificado por E.J 2010-04-22 para que tenga en cuenta hh:mm_ss
                    AND FEC_ALTA > p_fecha_desde
                    and FEC_ALTA <= p_fecha_hasta -- modificado por E.J 2011-01-21 para que tome las OE con fecha mayor que la fecha de cierre anterior y menor igual que la actual.
                 ) + (select nvl(sum(r.saldo_real), 0)
                        from suaf.t_rendicion r
                       where r.nro_rendicion = p_nro_actual - 1
                         and r.id_estado_rcch <> 16 -- anulada
                         and r.id_fondo_caja = v_ID_FONDO1)
               /* (SELECT nvl(sum(saldo), 0)
                FROM SUAF.T_BALANCE_SALDO          BC,
                     SUAF.T_FONDO_CAJACH           FCC,
                     SGP.T_UNIDADES_ADMINISTRATIVA UA
               WHERE BC.ID_FONDO_CAJA = FCC.ID_FONDO_CAJA
                 AND FCC.ID_UNIDAD_ADM = UA.ID_UNIDAD_ADM
                 AND BC.ID_FONDO_CAJA = v_ID_FONDO1
                 AND NRO_RENDICION = p_nro_actual - 1)*/
               
                TOTAL, -- es el campo subtotal que se muestra en el reporte (importe de las OE ingresadas +  saldo anterior )
               (select nvl(sum(r.saldo_real), 0)
                   from suaf.t_rendicion r
                  where r.nro_rendicion = p_nro_actual - 1
                    and r.id_estado_rcch <> 16 -- anulada
                    and r.id_fondo_caja = v_ID_FONDO1)
               
               /*(SELECT nvl(sum(saldo), 0)
                FROM SUAF.T_BALANCE_SALDO          BC,
                     SUAF.T_FONDO_CAJACH           FCC,
                     SGP.T_UNIDADES_ADMINISTRATIVA UA
               WHERE BC.ID_FONDO_CAJA = FCC.ID_FONDO_CAJA
                 AND FCC.ID_UNIDAD_ADM = UA.ID_UNIDAD_ADM
                 AND BC.ID_FONDO_CAJA = v_ID_FONDO1
                 AND NRO_RENDICION = p_nro_actual - 1) */ SALDO_ANTERIOR
          from vt_oefondos_03tc_CAB vt
         where ID_VIGENCIA = v_ID_VIGENCIA1
           AND ID_FONDO_CAJA = v_ID_FONDO1
              --          AND trunc(FEC_ALTA) between p_fecha_desde and p_fecha_hasta
              --           AND FEC_ALTA between p_fecha_desde and p_fecha_hasta -- modificado por E.J 2010-04-22 para que tenga en cuenta hh:mm_ss
           AND FEC_ALTA > p_fecha_desde
           and FEC_ALTA <= p_fecha_hasta -- modificado por E.J 2012-02-28 para que tome las OE con fecha mayor que la fecha de cierre anterior y menor igual que la actual.
         group by numero_unidad_adm,
                  unidad_adm,
                  numero_jurisdiccion,
                  jurisd,
                  ---agregado por LM----
                  NOMBRE_FUENTE_FIN,
                  ----------------------
                  año,
                  id_fondo_caja,
                  n_fondo,
                  letra_cod_f,
                  monto_fondo,
                  id_oe,
                  nro_oe,
                  fec_alta,
                  tipomovimiento,
                  nro_cta_ban,
                  denomi_cb,
                  importe_oe,
                  id_vigencia
         order by nro_oe;
    
    else
      OPEN p_T_CURSOR FOR
        SELECT v_numero_unidad_adm   NUMERO_UNIDAD_ADM,
               v_nombre_unidad_adm   UNIDAD_ADM,
               v_numero_jurisdiccion NUMERO_JURISDICCION,
               v_nombre_jurisdiccion JURISD,
               --agregado por LM----------------------
               v_nombre_fte_finan NOMBRE_FUENTE_FIN,
               ---------------------------------------
               round(v_ejercicio) AÑO,
               p_fecha_cierre_rp FECHA_CIERRE,
               p_nro_actual NRO_RENDICION,
               null CAJA,
               v_denominacion N_FONDO,
               v_letra_cod LETRA_COD_F,
               v_Resoluciones_fdo resoluciones,
               v_monto_fondo MONTO_FONDO,
               null ID_OE,
               null NRO_OE,
               to_date('01/01/1900', 'dd/mm/rrrr') FECHA_MOV_DIA,
               null TIPOMOVIMIENTO,
               null NRO_CTA_BAN,
               null DENOMI_CB,
               0 IMPORTE_OE,
               null ID_VIGENCIA,
               /*               (SELECT nvl(sum(saldo), 0)
                FROM SUAF.T_BALANCE_SALDO          BC,
                     SUAF.T_FONDO_CAJACH           FCC,
                     SGP.T_UNIDADES_ADMINISTRATIVA UA
               WHERE BC.ID_FONDO_CAJA = FCC.ID_FONDO_CAJA
                 AND FCC.ID_UNIDAD_ADM = UA.ID_UNIDAD_ADM
                 AND BC.ID_FONDO_CAJA = v_ID_FONDO1
                 AND NRO_RENDICION = p_nro_actual - 1)   TOTAL, -- cuando ingresa por aca el total es igual al saldo */
               
               (select nvl(sum(vt.IMPORTE_OE), 0)
                  from vt_oefondos_03tc_CAB vt
                 where ID_VIGENCIA = v_ID_VIGENCIA1
                   AND ID_FONDO_CAJA = v_ID_FONDO1
                      --                   AND FEC_ALTA between p_fecha_desde and p_fecha_hasta
                   AND FEC_ALTA > p_fecha_desde
                   and FEC_ALTA <= p_fecha_hasta -- modificado por E.J 2012-02-28 para que tome las OE con fecha mayor que la fecha de cierre anterior y menor igual que la actual.
                ) + (select nvl(sum(r.saldo_real), 0)
                       from suaf.t_rendicion r
                      where r.nro_rendicion = p_nro_actual - 1
                        and r.id_estado_rcch <> 16 -- anulada
                        and r.id_fondo_caja = v_ID_FONDO1) TOTAL, -- 02/08/2010
               
               (select nvl(sum(r.saldo_real), 0)
                   from suaf.t_rendicion r
                  where r.nro_rendicion = p_nro_actual - 1
                    and r.id_estado_rcch <> 16 -- anulada
                    and r.id_fondo_caja = v_ID_FONDO1)
               /*   (SELECT nvl(sum(saldo), 0)
                FROM SUAF.T_BALANCE_SALDO          BC,
                     SUAF.T_FONDO_CAJACH           FCC,
                     SGP.T_UNIDADES_ADMINISTRATIVA UA
               WHERE BC.ID_FONDO_CAJA = FCC.ID_FONDO_CAJA
                 AND FCC.ID_UNIDAD_ADM = UA.ID_UNIDAD_ADM
                 AND BC.ID_FONDO_CAJA = v_ID_FONDO1
                 AND NRO_RENDICION = p_nro_actual - 1)*/ SALDO_ANTERIOR
        
          from dual;
    
    end if;
  
    insert into suaf.t_control_reportes
      (nombre_reporte, fecha, usr_ing)
    values
      ('PKG_REPORTES_RUE.PRC_03_TC', sysdate, user);
    commit;
  
  END PRC_03_TC;






end PKG_Pruebas;
/

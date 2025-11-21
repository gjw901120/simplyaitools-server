package com.simply.ai.server.manager.manager;

import org.apache.ibatis.annotations.Param;

public interface ModelsManager {

    Integer getModelIdByName(@Param("modelName") String modelName);

}

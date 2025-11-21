package com.simply.ai.server.manager.manager.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.simply.ai.server.manager.entity.Models;
import com.simply.ai.server.manager.manager.ModelsManager;
import com.simply.ai.server.manager.mapper.ModelsMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class ModelsManagerImpl implements ModelsManager {

    @Resource
    private ModelsMapper modelsMapper;

    /**
     * 根据模型名称查询模型ID - 使用QueryWrapper
     */
    @Override
    public Integer getModelIdByName(String modelName) {
        if (modelName == null || modelName.trim().isEmpty()) {
            return null;
        }

        LambdaQueryWrapper<Models> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.select(Models::getId)
                .eq(Models::getName, modelName.trim())
                .eq(Models::getIsDel, 0);

        Models model = modelsMapper.selectOne(queryWrapper);
        return model != null ? model.getId() : null;
    }


}
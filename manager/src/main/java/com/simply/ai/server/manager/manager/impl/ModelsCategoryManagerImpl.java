package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.manager.ModelsCategoryManager;
import com.simply.ai.server.manager.mapper.ModelsCategoryMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class ModelsCategoryManagerImpl implements ModelsCategoryManager {

    @Resource
    private ModelsCategoryMapper modelsCategoryMapper;

}

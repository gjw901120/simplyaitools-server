package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.manager.ModelsPricingOnceManager;
import com.simply.ai.server.manager.mapper.ModelsPricingOnceMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class ModelsPricingOnceManagerImpl implements ModelsPricingOnceManager {

    @Resource
    private ModelsPricingOnceMapper modelsPricingOnceMapper;

}
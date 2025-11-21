package com.simply.ai.server.manager.manager.impl;

import com.simply.ai.server.manager.manager.NewsManager;
import com.simply.ai.server.manager.mapper.NewsMapper;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class NewsManagerImpl implements NewsManager {

    @Resource
    private NewsMapper newsMapper;

}
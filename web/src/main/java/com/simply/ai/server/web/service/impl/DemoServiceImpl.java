package com.simply.wallpaper.server.web.service.impl;

import com.simply.ai.server.manager.manager.DemoManager;
import com.simply.wallpaper.server.web.service.DemoService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class DemoServiceImpl implements DemoService {

    @Autowired
    private DemoManager demoManager;

    @Override
    public void sms() {
        demoManager.test();
    }
}

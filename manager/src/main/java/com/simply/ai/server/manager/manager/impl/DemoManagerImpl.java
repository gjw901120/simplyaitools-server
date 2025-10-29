package com.simply.ai.server.manager.manager.impl;

import cn.hutool.extra.spring.SpringUtil;
import com.simply.ai.server.manager.feign.client.DynamicClient;
import com.simply.ai.server.manager.manager.DemoManager;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.HashMap;

@Service
public class DemoManagerImpl implements DemoManager {

    @Value("${feign.api.order.url}")
    private String smsUrl;

    @Override
    public void test() {
        DynamicClient dynamicClient = SpringUtil.getBean(DynamicClient.class);
        Object result = dynamicClient.executePostApi("order", smsUrl, new HashMap<>(16));
        System.out.println(result);
    }
}
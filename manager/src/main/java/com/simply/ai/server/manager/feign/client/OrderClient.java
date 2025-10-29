package com.simply.ai.server.manager.feign.client;

import com.simply.common.core.entity.vo.ResponseResult;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;

@FeignClient(name = "order")
public interface OrderClient {

    @GetMapping(value = "/list")
    ResponseResult<?> list();
}
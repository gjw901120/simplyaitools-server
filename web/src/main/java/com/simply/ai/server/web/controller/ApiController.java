package com.simply.ai.server.web.controller;

import com.simply.common.core.entity.vo.ResponseResult;
import com.simply.wallpaper.server.web.service.DemoService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
@Slf4j
public class ApiController {

    @Autowired
    private DemoService demoService;

    @GetMapping(value = "/test")
    public ResponseResult<?> test() {
        //bombService.sms();
        return ResponseResult.success();
    }


}
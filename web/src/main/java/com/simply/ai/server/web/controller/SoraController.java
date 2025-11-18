package com.simply.ai.server.web.controller;


import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.service.SoraGenerateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.simply.common.core.entity.vo.ResponseResult;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/video")
public class SoraController {

    @Autowired
    private SoraGenerateService soraGenerateService;

    @PostMapping("/sora/generate")
    public ResponseResult<?> generate(@Valid @RequestBody SoraGenerateDTO request) {

        soraGenerateService.soraGenerate(request);

        return ResponseResult.success();
    }

    @PostMapping("/sora-pro/generate")
    public ResponseResult<?> generate(@Valid @RequestBody SoraProGenerateDTO request) {

        soraGenerateService.soraProGenerate(request);

        return ResponseResult.success();
    }


    @PostMapping("/sora/watermark-remover")
    public ResponseResult<?> watermarkRemover(@Valid @RequestBody SoraWatermarkRemoverDTO request) {

        soraGenerateService.soraWatermarkRemover(request);

        return ResponseResult.success();
    }

    @PostMapping("/sora-pro/storyboard")
    public ResponseResult<?> soraProStoryboard(@Valid @RequestBody SoraProStoryboardDTO request) {

        soraGenerateService.soraProStoryboard(request);

        return ResponseResult.success();
    }
}

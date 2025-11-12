package com.simply.ai.server.web.controller;

import com.simply.ai.server.web.config.exception.ResponseErrorType;
import com.simply.ai.server.web.model.dto.request.VeoGenerateDTO;
import com.simply.ai.server.web.service.VeoGenerateService;
import com.simply.common.core.exception.BaseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.simply.common.core.entity.vo.ResponseResult;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/video")
public class VideoController {
    @Autowired
    private VeoGenerateService veoGenerateService;

    @PostMapping("/veo/generate")
    public ResponseResult<?> generateVideo(@Valid @RequestBody VeoGenerateDTO request) {
        request.validateImageAndGenerationTypeWithException();

        request.validateModelAndGenerationTypeWithException();

        request.validateAspectRatioWithException();


        veoGenerateService.generateVideo(request);
        return ResponseResult.success();
    }
}

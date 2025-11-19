package com.simply.ai.server.web.controller;

import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.service.NanoBananaService;
import com.simply.ai.server.web.service.SunoService;
import com.simply.common.core.entity.vo.ResponseResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/audio/suno")
public class SunoController {

    @Autowired
    private SunoService sunoService;

    @PostMapping("/generate")
    public ResponseResult<?> generate(@Valid @RequestBody SunoGenerateDTO request) {

        sunoService.sunoGenerate(request);

        return ResponseResult.success();
    }

    @PostMapping("/extend")
    public ResponseResult<?> extend(@Valid @RequestBody SunoExtendDTO request) {

        sunoService.sunoExtend(request);

        return ResponseResult.success();
    }

    @PostMapping("/upload-cover")
    public ResponseResult<?> uploadCover(@Valid @RequestBody SunoUploadCoverDTO request) {

        sunoService.sunoUploadCover(request);

        return ResponseResult.success();
    }

    @PostMapping("/upload-extend")
    public ResponseResult<?> uploadExtend(@Valid @RequestBody SunoUploadExtendDTO request) {

        sunoService.sunoUploadExtend(request);

        return ResponseResult.success();
    }

    @PostMapping("/add-instrumental")
    public ResponseResult<?> addInstrumental(@Valid @RequestBody SunoAddInstrumentalDTO request) {

        sunoService.sunoAddInstrumental(request);

        return ResponseResult.success();
    }

    @PostMapping("/add-vocals")
    public ResponseResult<?> addVocals(@Valid @RequestBody SunoAddVocalsDTO request) {

        sunoService.sunoAddVocal(request);

        return ResponseResult.success();
    }

}

package com.simply.ai.server.web.controller;

import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.service.FluxKontextService;
import com.simply.ai.server.web.service.Gpt4oImageService;
import com.simply.ai.server.web.service.NanoBananaService;
import com.simply.common.core.entity.vo.ResponseResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/image")
public class ImageController {

    @Autowired
    private Gpt4oImageService gpt4oImageService;

    @Autowired
    private FluxKontextService fluxKontextService;

    @Autowired
    private NanoBananaService nanoBananaService;

    @PostMapping("/gpt4o-image/generate")
    public ResponseResult<?> gpt4oImageGenerate(@Valid @RequestBody Gpt4oImageGenerateDTO request) {

        gpt4oImageService.gpt4oImageGenerate(request);

        return ResponseResult.success();
    }

    @PostMapping("/flux-kontext/generate")
    public ResponseResult<?> fluxKontextGenerate(@Valid @RequestBody FluxKontextGenerateDTO request) {

        fluxKontextService.fluxKontextGenerate(request);

        return ResponseResult.success();
    }

    @PostMapping("nano-banana/generate")
    public ResponseResult<?> nanoBananaGenerate(@Valid @RequestBody NanoBananaGenerateDTO request) {

        nanoBananaService.nanoBananaGenerate(request);

        return ResponseResult.success();
    }

    @PostMapping("/nano-banana/edit")
    public ResponseResult<?> nanoBananaEdit(@Valid @RequestBody NanoBananaEditDTO request) {

        nanoBananaService.nanoBananaEdit(request);

        return ResponseResult.success();
    }

}

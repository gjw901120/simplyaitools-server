package com.simply.ai.server.web.controller;

import com.simply.ai.server.web.model.dto.request.ElevenlabsAudioIsolationDTO;
import com.simply.ai.server.web.model.dto.request.ElevenlabsSTTDTO;
import com.simply.ai.server.web.model.dto.request.ElevenlabsSoundEffectDTO;
import com.simply.ai.server.web.model.dto.request.ElevenlabsTTSDTO;
import com.simply.ai.server.web.service.ElevenlabsService;
import com.simply.common.core.entity.vo.ResponseResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping("/api/audio/elevenLabs")
public class ElevenLabsController {

    @Autowired
    private ElevenlabsService elevenlabsService;

    @PostMapping("/text-to-speech")
    public ResponseResult<?> textToSpeech(@Valid @RequestBody ElevenlabsTTSDTO request) {

        elevenlabsService.elevenlabsTTS(request);

        return ResponseResult.success();
    }

    @PostMapping("/speech-to-text")
    public ResponseResult<?> speechToText(@Valid @RequestBody ElevenlabsSTTDTO request) {

        elevenlabsService.elevenlabsSTT(request);

        return ResponseResult.success();
    }

    @PostMapping("/sound-effect-v2")
    public ResponseResult<?> soundEffectV2(@Valid @RequestBody ElevenlabsSoundEffectDTO request) {

        elevenlabsService.elevenlabsSoundEffectDTO(request);

        return ResponseResult.success();
    }

    @PostMapping("/audio-isolation")
    public ResponseResult<?> audioIsolation(@Valid @RequestBody ElevenlabsAudioIsolationDTO request) {

        elevenlabsService.elevenlabsAudioIsolationDTO(request);

        return ResponseResult.success();
    }


}

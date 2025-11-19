package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.ElevenLabsModelEnum;
import com.simply.ai.server.manager.enums.ElevenLabsResponseCodeEnum;
import com.simply.ai.server.manager.manager.ElevenLabsManager;
import com.simply.ai.server.manager.model.request.ElevenLabsAudioIsolationRequest;
import com.simply.ai.server.manager.model.request.ElevenLabsSTTRequest;
import com.simply.ai.server.manager.model.request.ElevenLabsSoundEffectRequest;
import com.simply.ai.server.manager.model.request.ElevenLabsTTSRequest;
import com.simply.ai.server.manager.model.response.ElevenLabsResponse;
import com.simply.ai.server.web.model.dto.request.ElevenlabsAudioIsolationDTO;
import com.simply.ai.server.web.model.dto.request.ElevenlabsSTTDTO;
import com.simply.ai.server.web.model.dto.request.ElevenlabsSoundEffectDTO;
import com.simply.ai.server.web.model.dto.request.ElevenlabsTTSDTO;
import com.simply.ai.server.web.service.ElevenlabsService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ElevenlabsServiceImpl implements ElevenlabsService {

    @Autowired
    private ElevenLabsManager elevenLabsManager;

    @Override
    public void elevenlabsTTS(ElevenlabsTTSDTO elevenlabsTTSDTO) {

        // 实现视频生成逻辑
        ElevenLabsTTSRequest request = new ElevenLabsTTSRequest();

        ElevenLabsTTSRequest.TTSInput input = new ElevenLabsTTSRequest.TTSInput();

        request.setModel(elevenlabsTTSDTO.getModel());

        BeanUtils.copyProperties(elevenlabsTTSDTO, input);

        request.setInput(input);

        ElevenLabsResponse response = elevenLabsManager.textToSpeech(request);

        if(!ElevenLabsResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }

    @Override
    public void elevenlabsSTT(ElevenlabsSTTDTO elevenlabsSTTDTO) {

        // 实现视频生成逻辑
        ElevenLabsSTTRequest request = new ElevenLabsSTTRequest();

        ElevenLabsSTTRequest.STTInput input = new ElevenLabsSTTRequest.STTInput();

        request.setModel(elevenlabsSTTDTO.getModel());

        BeanUtils.copyProperties(elevenlabsSTTDTO, input);

        request.setInput(input);

        ElevenLabsResponse response = elevenLabsManager.speechToText(request);

        if(!ElevenLabsResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }

    @Override
    public void elevenlabsAudioIsolationDTO(ElevenlabsAudioIsolationDTO elevenlabsAudioIsolationDTO) {

        // 实现视频生成逻辑
        ElevenLabsAudioIsolationRequest request = new ElevenLabsAudioIsolationRequest();

        ElevenLabsAudioIsolationRequest.AudioIsolationInput input = new ElevenLabsAudioIsolationRequest.AudioIsolationInput();

        request.setModel(elevenlabsAudioIsolationDTO.getModel());

        BeanUtils.copyProperties(elevenlabsAudioIsolationDTO, input);

        request.setInput(input);

        ElevenLabsResponse response = elevenLabsManager.isolateAudio(request);

        if(!ElevenLabsResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }

    @Override
    public void elevenlabsSoundEffectDTO(ElevenlabsSoundEffectDTO elevenlabsSoundEffectDTO) {

        // 实现视频生成逻辑
        ElevenLabsSoundEffectRequest request = new ElevenLabsSoundEffectRequest();

        ElevenLabsSoundEffectRequest.SoundEffectInput input = new ElevenLabsSoundEffectRequest.SoundEffectInput();

        request.setModel(elevenlabsSoundEffectDTO.getModel());

        BeanUtils.copyProperties(elevenlabsSoundEffectDTO, input);

        request.setInput(input);

        ElevenLabsResponse response = elevenLabsManager.generateSoundEffect(request);

        if(!ElevenLabsResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMessage());
        }

    }

}

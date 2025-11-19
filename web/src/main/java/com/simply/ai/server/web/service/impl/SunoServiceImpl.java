package com.simply.ai.server.web.service.impl;

import com.simply.ai.server.manager.enums.SunoResponseCodeEnum;
import com.simply.ai.server.manager.manager.SunoManger;
import com.simply.ai.server.manager.model.request.*;
import com.simply.ai.server.manager.model.response.SunoMusicResponse;
import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.service.SunoService;
import com.simply.common.core.exception.BaseException;
import com.simply.common.core.exception.error.ThirdpartyErrorType;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SunoServiceImpl implements SunoService {

    @Autowired
    private SunoManger sunoManger;

    @Override
    public void sunoGenerate(SunoGenerateDTO sunoGenerateDTO) {

        // 实现视频生成逻辑
        SunoGenerateRequest request = new SunoGenerateRequest();

        BeanUtils.copyProperties(sunoGenerateDTO, request);

        SunoMusicResponse response = sunoManger.generateMusic(request);

        if(!SunoResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }

    @Override
    public void sunoExtend(SunoExtendDTO sunoExtendDTO) {

        // 实现视频生成逻辑
        SunoExtendRequest request = new SunoExtendRequest();

        BeanUtils.copyProperties(sunoExtendDTO, request);

        SunoMusicResponse response = sunoManger.extendMusic(request);

        if(!SunoResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }

    @Override
    public void sunoUploadCover(SunoUploadCoverDTO sunoUploadCoverDTO) {

        // 实现视频生成逻辑
        SunoUploadCoverRequest request = new SunoUploadCoverRequest();

        BeanUtils.copyProperties(sunoUploadCoverDTO, request);

        SunoMusicResponse response = sunoManger.uploadCover(request);

        if(!SunoResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }

    @Override
    public void sunoAddVocal(SunoAddVocalsDTO sunoAddVocalsDTO) {

        // 实现视频生成逻辑
        SunoAddVocalsRequest request = new SunoAddVocalsRequest();

        BeanUtils.copyProperties(sunoAddVocalsDTO, request);

        SunoMusicResponse response = sunoManger.addVocals(request);

        if(!SunoResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }

    @Override
    public void sunoUploadExtend(SunoUploadExtendDTO sunoUploadExtendDTO) {

        // 实现视频生成逻辑
        SunoUploadExtendRequest request = new SunoUploadExtendRequest();

        BeanUtils.copyProperties(sunoUploadExtendDTO, request);

        SunoMusicResponse response = sunoManger.uploadExtend(request);

        if(!SunoResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }

    @Override
    public void sunoAddInstrumental(SunoAddInstrumentalDTO sunoAddInstrumentalDTO) {

        // 实现视频生成逻辑
        SunoAddInstrumentalRequest request = new SunoAddInstrumentalRequest();

        BeanUtils.copyProperties(sunoAddInstrumentalDTO, request);

        SunoMusicResponse response = sunoManger.addInstrumental(request);

        if(!SunoResponseCodeEnum.SUCCESS.equals(response.getCode())) {
            throw new BaseException(ThirdpartyErrorType.THIRDPARTY_SERVER_ERROR, response.getMsg());
        }

    }


}

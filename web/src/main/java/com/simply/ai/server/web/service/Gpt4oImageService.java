package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.image.Gpt4oImageGenerateDTO;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface Gpt4oImageService {

    BaseResponse gpt4oImageGenerate(Gpt4oImageGenerateDTO request);

}

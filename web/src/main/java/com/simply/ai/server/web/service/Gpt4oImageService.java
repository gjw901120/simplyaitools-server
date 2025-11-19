package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.Gpt4oImageGenerateDTO;

public interface Gpt4oImageService {

    void gpt4oImageGenerate(Gpt4oImageGenerateDTO request);

}

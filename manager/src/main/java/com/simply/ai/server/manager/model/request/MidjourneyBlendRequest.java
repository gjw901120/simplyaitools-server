package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.constant.MidjourneyConstant;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serial;
import java.util.List;

/**
 * Blend 任务请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class MidjourneyBlendRequest extends MidjourneyBaseRequest {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 要混合的图片 base64 编码数组
     */
    @NotNull(message = "图片数组不能为空")
    @Size(min = MidjourneyConstant.MIN_BLEND_IMAGES, max = MidjourneyConstant.MAX_BLEND_IMAGES,
            message = "图片数量必须在" + MidjourneyConstant.MIN_BLEND_IMAGES + "到" + MidjourneyConstant.MAX_BLEND_IMAGES + "之间")
    private List<@NotBlank String> base64Array;

    /**
     * 输出图片的宽高比设置
     */
    private Dimensions dimensions = Dimensions.SQUARE;

    /**
     * 图片尺寸枚举（内部枚举）
     */
    public enum Dimensions {
        PORTRAIT("PORTRAIT", "2:3 比例", "竖版"),
        SQUARE("SQUARE", "1:1 比例", "正方形"),
        LANDSCAPE("LANDSCAPE", "3:2 比例", "横版");

        private final String code;
        private final String description;
        private final String type;

        Dimensions(String code, String description, String type) {
            this.code = code;
            this.description = description;
            this.type = type;
        }

        public String getCode() {
            return code;
        }

        public String getDescription() {
            return description;
        }

        public String getType() {
            return type;
        }

        public static Dimensions getByCode(String code) {
            for (Dimensions value : values()) {
                if (value.getCode().equals(code)) {
                    return value;
                }
            }
            return null;
        }
    }

    /**
     * 构建 Blend 请求
     */
    public static MidjourneyBlendRequest build(List<String> base64Array, Dimensions dimensions) {
        MidjourneyBlendRequest request = new MidjourneyBlendRequest();
        request.setBase64Array(base64Array);
        request.setDimensions(dimensions);
        return request;
    }

    /**
     * 流式构建方法
     */
    public static MidjourneyBlendRequest create(List<String> base64Array) {
        return new MidjourneyBlendRequest().withBase64Array(base64Array);
    }

    /**
     * 设置图片数组
     */
    public MidjourneyBlendRequest withBase64Array(List<String> base64Array) {
        this.base64Array = base64Array;
        return this;
    }

    /**
     * 设置尺寸
     */
    public MidjourneyBlendRequest withDimensions(Dimensions dimensions) {
        this.dimensions = dimensions;
        return this;
    }

    /**
     * 添加单个图片
     */
    public MidjourneyBlendRequest addBase64Image(String base64Image) {
        if (this.base64Array == null) {
            this.base64Array = new java.util.ArrayList<>();
        }
        this.base64Array.add(base64Image);
        return this;
    }

    /**
     * 验证业务规则
     */
    public void validateBusinessRules() {
        if (base64Array == null || base64Array.size() < MidjourneyConstant.MIN_BLEND_IMAGES ||
                base64Array.size() > MidjourneyConstant.MAX_BLEND_IMAGES) {
            throw new IllegalArgumentException("图片数量必须在" + MidjourneyConstant.MIN_BLEND_IMAGES +
                    "到" + MidjourneyConstant.MAX_BLEND_IMAGES + "之间");
        }
    }
}